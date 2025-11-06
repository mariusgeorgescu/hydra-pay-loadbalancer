{-# LANGUAGE OverloadedStrings #-}

module HydraPay.ServiceScanner
  ( Service(..)
  , serviceBaseUrl
  , serviceName
  , scanAvailableServices
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (catch)
import Control.Monad (filterM)
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Types (statusCode)

-- | Service configuration
data Service = Service
  { serviceName :: String
  , serviceHost :: String
  , servicePort :: Int
  }
  deriving (Eq, Show)

-- | Get base URL for a service
serviceBaseUrl :: Service -> String
serviceBaseUrl (Service _ host port) = "http://" ++ host ++ ":" ++ show port

-- | Check if a service is available by making a test HTTP request
checkServiceAvailable :: Service -> IO Bool
checkServiceAvailable service = do
  let baseUrl = serviceBaseUrl service
      testUrl = baseUrl ++ "/state?id=test-availability-check"

  manager <- HTTP.newManager HTTP.defaultManagerSettings
  request <- HTTP.parseRequest testUrl

  -- Set a short timeout (2 seconds)
  let requestWithTimeout = request
        { HTTP.responseTimeout = HTTP.responseTimeoutMicro 2000000  -- 2 seconds
        }

  catch
    (do
      response <- HTTP.httpLbs requestWithTimeout manager
      -- If we get any HTTP response (even 400/404), the service is available
      let status = HTTP.responseStatus response
      return $ statusCode status >= 200 && statusCode status < 600)
    (\e -> do
      -- Connection errors mean service is not available
      -- Catch all exceptions (HttpException, IOException, etc.)
      let _ = e :: HTTP.HttpException
      return False)

-- | Scan ports from startPort to endPort (inclusive) and return available services
scanAvailableServices :: String -> Int -> Int -> IO [Service]
scanAvailableServices host startPort endPort = do
  let ports = [startPort .. endPort]
      services = map (\port -> Service ("Blazar-Pay Service: " ++ show port) host port) ports

  -- Check services sequentially with a small delay to avoid overwhelming the system
  filterM checkWithDelay services
  where
    checkWithDelay service = do
      available <- checkServiceAvailable service
      threadDelay 50000  -- 50ms delay between checks
      return available

