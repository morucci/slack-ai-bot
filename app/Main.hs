{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

-- https://github.com/ggml-org/llama.cpp/blob/master/examples/server/README.md

import Control.Monad (forever)
import Data.Aeson
import Data.Aeson.Types (Parser, parseMaybe)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL

import Data.Maybe
import Data.Text as T (Text)
import qualified Data.Text as T
import GHC.Generics
import Llama
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.WebSockets hiding (requestHeaders)
import System.Environment
import Web.Slack
import Web.Slack.Chat
import qualified Web.Slack.Common
import Web.Slack.Conversation
import Web.Slack.Types
import Wuss (runSecureClient)

data SlackWSEventEvent = SlackWSEventEvent
    { seUser :: Text
    , seText :: Text
    , seType :: Text
    , seThreadTs :: Text
    , seChannel :: Text
    }
    deriving (Show, Generic)

data SlackWSEventPayload = SlackWSEventPayload
    { seEvent :: SlackWSEventEvent
    }
    deriving (Show, Generic)

data SlackWSEvent = SlackWSEvent
    { seEnvelopeId :: Maybe Text
    , sePayload :: Maybe SlackWSEventPayload
    }
    deriving (Show, Generic)

wsEventKeyModifier :: String -> String
wsEventKeyModifier = (camelTo2 '_' . dropPrefix)
  where
    dropPrefix s =
        let st = T.pack s
         in T.unpack (T.drop (T.length "se") st)

wsJSONEventParseOptions :: Options
wsJSONEventParseOptions = defaultOptions{fieldLabelModifier = wsEventKeyModifier}

instance FromJSON SlackWSEvent where
    parseJSON = genericParseJSON wsJSONEventParseOptions

instance FromJSON SlackWSEventPayload where
    parseJSON = genericParseJSON wsJSONEventParseOptions

instance FromJSON SlackWSEventEvent where
    parseJSON = genericParseJSON wsJSONEventParseOptions

resume = do
    rM1 <-
        llamaTemplated
            "http://localhost:8080"
            ( LlamaApplyTemplateRequest
                [ LlamaMessage System "You are fast as possible in your reply and be the succint as possible to summarize my text requests. You can use bullet points for example."
                , LlamaMessage User "Could you sumarize this: In May 2017 Rafael Correa became the first President in more than two decades to serve out his complete terms in office since Sixto Durán Ballén, who served from 1992 to 1996. Before Correa, a period of deep political instability from 1996 to 2006 also saw a grave economic crisis in 1998-2000. During this time, Durán Ballén's three elected successors, Abdalá Bucaram, Jamil Mahuad and Lucio Gutiérrez, were deposed in popular revolts, followed by military or legislative coups d'États, in 1997, 2000, and 2005, respectively. Since Correa, Lenín Moreno (2017–2021) has also completed a full 4-year presidential term, despite a large 2019 popular revolt that nearly toppled his government. "
                ]
            )
    print rM1

getEnvs = do
    sAppToken <- lookupEnv "SLACK_APP_TOKEN"
    sBotToken <- lookupEnv "SLACK_BOT_TOKEN"
    pure (sAppToken, sBotToken)

sendSlack :: Text -> Maybe Text -> Text -> IO ()
sendSlack channel threadTs message = do
    (_, (Just token)) <- getEnvs
    config <- mkSlackConfig $ T.pack token
    let msg = (mkPostMsgReq channel message){postMsgReqThreadTs = threadTs}
    ret <- chatPostMessage config msg
    print ret
    pure ()

getWebSocketUrl :: IO String
getWebSocketUrl = do
    manager <- newManager tlsManagerSettings
    ((Just token), _) <- getEnvs
    let request = "https://slack.com/api/apps.connections.open"
    initialRequest <- parseRequest request
    let authRequest =
            initialRequest
                { method = "POST"
                , requestHeaders =
                    [ ("Authorization", B.pack ("Bearer " ++ token))
                    , ("Content-type", "application/x-www-form-urlencoded")
                    ]
                }
    response <- httpLbs authRequest manager
    print response
    let body = responseBody response
    case eitherDecode body of
        Right (Object obj) ->
            case parseMaybe (.: "url") obj of
                Just wsUrl -> return $ T.unpack wsUrl
                _ -> do
                    print obj
                    error "Could not find WebSocket URL in response"
        _ -> error "Invalid response from Slack"

createPrompt :: [Web.Slack.Common.Message] -> Text
createPrompt = addMessageToPrompt basePrompt
  where
    basePrompt = "Here is the conversation that you need to summarize:\n"
    buildNewPrompt :: Text -> Web.Slack.Common.SlackMessageText -> Text
    buildNewPrompt prompt' (Web.Slack.Common.SlackMessageText msg) = prompt' <> msg <> "\n"
    addMessageToPrompt :: Text -> [Web.Slack.Common.Message] -> Text
    addMessageToPrompt prompt' msgs = case msgs of
        (msg : rest) ->
            let newPrompt = buildNewPrompt prompt' (Web.Slack.Common.messageText msg)
             in addMessageToPrompt newPrompt rest
        [] -> prompt'

runSlackSocket :: String -> String -> IO ()
runSlackSocket host' path' = runSecureClient host' 443 path' $ \conn -> do
    let botId = "<@U08NULM0SJH>"
    putStrLn "Connected to Slack via Socket Mode WebSocket"
    forever $ do
        putStrLn "Waiting for event ..."
        msg <- receiveData conn
        putStrLn $ "Events received (raw): " ++ BL.unpack msg
        case decodeSlackWSEvent msg of
            Right e -> case seEnvelopeId e of
                Just eId -> do
                    let ack = object ["envelope_id" .= (eId :: Text)]
                    sendTextData conn (encode ack)
                    putStrLn $ "Decoded event: " ++ show e
                    case sePayload e of
                        Just evPayload -> do
                            if T.isInfixOf botId (seText $ seEvent evPayload)
                                then do
                                    putStrLn "Bot mentionned ! let get the thread replies"
                                    replies <- getThreadReplies (seChannel $ seEvent evPayload) $ seThreadTs $ seEvent evPayload
                                    putStrLn $ show replies
                                    print $ createPrompt $ fromMaybe (error "No history") replies
                                    -- TODO query the LLM
                                    -- TODO send back the LLM reply
                                    pure ()
                                else do
                                    putStrLn "Skipping the message because of no mention of the bot"
                        Nothing -> putStrLn "No payload - skipping"
                Nothing -> do
                    putStrLn "Skipping message w/o envelope id"
            Left err -> putStrLn $ "Unable to decode: " ++ show err
  where
    decodeSlackWSEvent :: BL.ByteString -> Either String SlackWSEvent
    decodeSlackWSEvent = eitherDecode

parseWssUrl :: String -> (String, String)
parseWssUrl fullUrl =
    let noWss = drop (T.length "wss://") fullUrl
        (host', path') = break (== '/') noWss
     in (host', if null path' then "/" else path')

getThreadReplies :: Text -> Text -> IO (Maybe [Web.Slack.Common.Message])
getThreadReplies channel ts = do
    -- https://api.slack.com/methods/conversations.replies/test
    (_, (Just token)) <- getEnvs
    let ts' = case timestampFromText ts of
            Right r -> r
            Left _ -> error "Unable to parse TS"
    config <- mkSlackConfig $ T.pack token
    let req = mkRepliesReq (ConversationId channel) ts'
    resp <- conversationsReplies config req
    case resp of
        Right d -> pure $ Just $ historyRspMessages d
        Left err -> do
            putStrLn $ "Unable to fetch replies" ++ show err
            pure Nothing

-- | Entry point
main :: IO ()
main = do
    wsUrl <- getWebSocketUrl
    let (host', path') = parseWssUrl wsUrl
    putStrLn $ "Connecting to: " ++ wsUrl
    runSlackSocket host' path'

-- Take the thread_ts from the event where the bot has been pinged.
-- getThreadReplies "C08P6DFRRMX" "1745256051.471189"
-- sendSlack "ai-bot" (Just "1745256051.471189") "Hello to thread"
-- pure ()
