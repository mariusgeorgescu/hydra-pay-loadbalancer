{-# LANGUAGE OverloadedStrings #-}



module HydraPay.Client.Logger
  ( logRequest
  , logResponse
  , logError
  ) where

import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson.Types ()
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Network.HTTP.Client
import Network.HTTP.Types (statusCode)

-- | Log an outgoing request
logRequest :: Request -> Maybe ByteString -> IO ()
logRequest req body = do
  timestamp <- getCurrentTime
  let timeStr = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" timestamp
      methodStr = show $ method req
      urlStr = TE.decodeUtf8 $ path req
      bodyStr = maybe "No body" TLE.decodeUtf8 body
  putStrLn $ "[" ++ timeStr ++ "] REQUEST: " ++ methodStr ++ " " ++ show urlStr
  case body of
    Nothing -> return ()
    Just b -> do
      case decode b of
        Nothing -> putStrLn $ "  Body: " ++ show bodyStr
        Just (v :: Value) -> putStrLn $ "  Body: " ++ show (TLE.decodeUtf8 $ encodePretty v)

-- | Log an incoming response
logResponse :: Response ByteString -> IO ()
logResponse resp = do
  timestamp <- getCurrentTime
  let timeStr = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" timestamp
      status = statusCode $ responseStatus resp
      body = responseBody resp
  putStrLn $ "[" ++ timeStr ++ "] RESPONSE: " ++ show status
  case decode body of
    Nothing -> putStrLn $ "  Body: " ++ show (TLE.decodeUtf8 body)
    Just (v :: Value) -> putStrLn $ "  Body: " ++ show (TLE.decodeUtf8 $ encodePretty v)

-- | Log an error
logError :: Text -> IO ()
logError err = do
  timestamp <- getCurrentTime
  let timeStr = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" timestamp
  putStrLn $ "[" ++ timeStr ++ "] ERROR: " ++ show err
