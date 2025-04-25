{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forever)
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL

import Data.Maybe
import Data.Text as T
import GHC.Generics
import Llama
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.WebSockets hiding (requestHeaders)
import System.Environment
import Web.Slack
import Web.Slack.Chat
import Web.Slack.Common
import Web.Slack.Conversation
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

resume :: Text -> IO (Maybe Text)
resume prompt' =
    llamaTemplated
        "http://rdev:8080"
        ( LlamaApplyTemplateRequest
            [ LlamaMessage System "You are a concise and efficient assistant. Your job is to summarize group chat conversations without adding unnecessary detail. Always preserve usernames, and highlight only the key points, agreements, or decisions made. Keep it short and direct."
            , LlamaMessage User prompt'
            ]
        )

getEnvs :: IO (Maybe String, Maybe String, Maybe String)
getEnvs = do
    sAppToken <- lookupEnv "SLACK_APP_TOKEN"
    sBotToken <- lookupEnv "SLACK_BOT_TOKEN"
    sBotId <- lookupEnv "SLACK_BOT_ID"
    pure (sAppToken, sBotToken, sBotId)

sendSlack :: String -> Text -> Maybe Text -> Text -> IO ()
sendSlack token channel threadTs message = do
    config <- mkSlackConfig $ T.pack token
    let msg = (mkPostMsgReq channel message){postMsgReqThreadTs = threadTs}
    ret <- chatPostMessage config msg
    print ret

getWebSocketUrl :: String -> IO String
getWebSocketUrl token = do
    manager <- newManager tlsManagerSettings
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
    let body = responseBody response
    case eitherDecode body of
        Right (Object obj) ->
            case parseMaybe (.: "url") obj of
                Just wsUrl -> return $ T.unpack wsUrl
                _ -> error "Could not find WebSocket URL in response"
        _ -> error "Invalid response from Slack"

mkBotMention :: Text -> Text
mkBotMention botId = "<@" <> botId <> ">"

createPrompt :: Text -> [Web.Slack.Common.Message] -> Text
createPrompt botId = addMessageToPrompt basePrompt
  where
    botMention = mkBotMention botId
    basePrompt = "Summarize the following group chat conversation. Focus on the key points each user made, and include any important decisions or agreements. Refer to users by their usernames. Keep the summary short and efficient.:\n\n"
    buildNewPrompt :: Text -> SlackMessageText -> Text -> Text
    buildNewPrompt prompt' (SlackMessageText msg) user = prompt' <> "[" <> user <> "]: " <> msg <> "\n\n"
    addMessageToPrompt :: Text -> [Web.Slack.Common.Message] -> Text
    addMessageToPrompt prompt' msgs = case msgs of
        (msg : rest) ->
            let newPrompt = case Web.Slack.Common.messageUser msg of
                    Just (Web.Slack.Common.UserId user) ->
                        if user /= botId && not (T.isInfixOf botMention (Web.Slack.Common.unSlackMessageText $ Web.Slack.Common.messageText msg))
                            then buildNewPrompt prompt' (Web.Slack.Common.messageText msg) user
                            else prompt'
                    Nothing -> prompt'
             in addMessageToPrompt newPrompt rest
        [] -> prompt'

runSlackSocket :: String -> String -> String -> String -> IO ()
runSlackSocket sBotToken sBotId host' path' = runSecureClient host' 443 path' $ \conn -> do
    let botMention = mkBotMention $ T.pack sBotId
    putStrLn "Connected to Slack via Socket Mode WebSocket"
    forever $ do
        putStrLn "Waiting for event ..."
        msg <- receiveData conn
        putStrLn $ "Events received (raw): " ++ BL.unpack msg
        case decodeSlackWSEvent msg of
            Right e -> case seEnvelopeId e of
                Just eId -> do
                    let ack = object ["envelope_id" .= (eId :: Text)]
                    print $ "Sending ack: " ++ show ack
                    sendTextData conn (encode ack)
                    putStrLn $ "Decoded event: " ++ show e
                    case sePayload e of
                        Just evPayload -> do
                            if T.isInfixOf botMention (seText $ seEvent evPayload)
                                then do
                                    putStrLn "Bot mentionned ! let get the thread replies"
                                    let channel = seChannel $ seEvent evPayload
                                        threadTs = seThreadTs $ seEvent evPayload
                                    replies <- getThreadReplies sBotToken channel threadTs
                                    -- putStrLn $ show replies
                                    let prompt' = createPrompt (T.pack sBotId) $ fromMaybe (error "No history") replies
                                    -- putStrLn $ T.unpack prompt'
                                    _ <- sendSlack sBotToken channel (Just threadTs) "Computing the thread summary ..."
                                    mSummary <- resume prompt'
                                    case mSummary of
                                        Just summary -> sendSlack sBotToken channel (Just threadTs) summary
                                        Nothing -> sendSlack sBotToken channel (Just threadTs) "I'm sorry but I was unable to summarize this thread."
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
    let noWss = Prelude.drop (T.length "wss://") fullUrl
        (host', path') = Prelude.break (== '/') noWss
     in (host', if Prelude.null path' then "/" else path')

getThreadReplies :: String -> Text -> Text -> IO (Maybe [Web.Slack.Common.Message])
getThreadReplies token channel ts = do
    -- https://api.slack.com/methods/conversations.replies/test
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
    (Just sAPPToken, Just sBotToken, Just sBotId) <- getEnvs
    wsUrl <- getWebSocketUrl sAPPToken
    let (host', path') = parseWssUrl wsUrl
    putStrLn $ "Connecting to: " ++ wsUrl
    runSlackSocket sBotToken sBotId host' path'
