{-# LANGUAGE OverloadedStrings #-}

module Main where

import Llama

-- https://github.com/ggml-org/llama.cpp/blob/master/examples/server/README.md

import Control.Concurrent (forkIO)
import Control.Monad (forever)
import Data.Aeson

-- import Data.Aeson.KeyMap as AKM
import Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types
import Wuss (runSecureClient)

import Network.WebSockets hiding (requestHeaders)
import System.Environment
import Web.Slack
import Web.Slack.Chat

resume = do
    -- rM <- llama "http://rdev:8081" "Hello my master"
    -- print rM
    rM2 <-
        llamaTemplated
            "http://localhost:8080"
            ( LlamaApplyTemplateRequest
                [ LlamaMessage System "You are fast as possible in your reply and be the succint as possible to summarize my text requests. You can use bullet points for example."
                , LlamaMessage User "Could you sumarize this: In May 2017 Rafael Correa became the first President in more than two decades to serve out his complete terms in office since Sixto Durán Ballén, who served from 1992 to 1996. Before Correa, a period of deep political instability from 1996 to 2006 also saw a grave economic crisis in 1998-2000. During this time, Durán Ballén's three elected successors, Abdalá Bucaram, Jamil Mahuad and Lucio Gutiérrez, were deposed in popular revolts, followed by military or legislative coups d'États, in 1997, 2000, and 2005, respectively. Since Correa, Lenín Moreno (2017–2021) has also completed a full 4-year presidential term, despite a large 2019 popular revolt that nearly toppled his government. "
                ]
            )
    print rM2

getEnvs = do
    sAppToken <- lookupEnv "SLACK_APP_TOKEN"
    sBotToken <- lookupEnv "SLACK_BOT_TOKEN"
    pure (sAppToken, sBotToken)

sendSlack = do
    (_, (Just token)) <- getEnvs
    config <- mkSlackConfig $ T.pack token
    let msg = mkPostMsgReq "ai-bot" "test message"
    ret <- chatPostMessage config msg
    print ret
    pure ()

-- | Get WebSocket URL from Slack
getWebSocketUrl :: IO String
getWebSocketUrl = do
    manager <- newManager tlsManagerSettings
    ((Just token), _) <- getEnvs
    let request = "https://slack.com/api/apps.connections.open"
    initialRequest <- parseRequest request
    let authRequest =
            initialRequest
                { method = "POST"
                , requestHeaders = [("Authorization", B.pack ("Bearer " ++ token)), ("Content-type", "application/x-www-form-urlencoded")]
                }
    response <- httpLbs authRequest manager
    print response
    let body = responseBody response
    case eitherDecode body of
        Right (Object obj) ->
            case parseMaybe (.: "url") obj of
                Just wsUrl -> return $ T.unpack wsUrl
                z@(_) -> do
                    print obj
                    error "Could not find WebSocket URL in response"
        _ -> error "Invalid response from Slack"

-- | WebSocket client
runSlackSocket :: String -> String -> IO ()
runSlackSocket host' path' = runSecureClient host' 443 path' $ \conn -> do
    putStrLn "Connected to Slack via Socket Mode WebSocket"
    forever $ do
        putStrLn "Waiting for event ..."
        msg <- receiveData conn
        putStrLn "----- Received Event -----"
        BL.putStrLn msg
        case decode msg of
            Just (Object obj) -> case parseMaybe (.: "envelope_id") obj of
                Just envelopeId -> do
                    let ack = object ["envelope_id" .= (envelopeId :: Text)]
                    print ack
                    sendTextData conn (encode ack)
                _ -> do
                    print "no envelope id"
                    return ()
            _ -> do
                print "Unable to decode"
                print msg

parseWssUrl :: String -> (String, String)
parseWssUrl fullUrl =
    let noWss = drop (T.length "wss://") fullUrl
        (host', path') = break (== '/') noWss
     in (host', if null path' then "/" else path')

-- | Entry point
main :: IO ()
main = do
    wsUrl <- getWebSocketUrl
    let (host', path') = parseWssUrl wsUrl
    putStrLn $ "Connecting to: " ++ wsUrl
    runSlackSocket host' path'
