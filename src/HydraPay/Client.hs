{-# LANGUAGE OverloadedStrings #-}

module HydraPay.Client
  (   -- * Client functions
    queryFunds
  , deposit
  , withdraw
  , payMerchant
  , openHead
  , closeHead
  , getHeadState
    
  -- * Client runner
  , runHydraClient
  , HydraClientError(..)
  , formatError
  
  -- * Re-exports
  , module HydraPay.API.Types
  ) where

import Control.Exception (try)
import Data.Aeson (decode, encode)
import Data.Char (toLower)
import Data.IORef
import Data.List (isInfixOf, isPrefixOf)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Client.TLS
import Network.HTTP.Types (methodPost, statusCode)
import Network.HTTP.Types.Header (HeaderName, RequestHeaders)
import Servant.Client
import Servant.Client.Core (ClientError(..))
import Servant (NoContent)
import Servant.API

import HydraPay.API
import HydraPay.API.Types

-- | Client error type
data HydraClientError
  = HydraClientHttpError ClientError [String]
  | HydraClientException String
  deriving (Eq, Show)

-- | Generated client functions
queryFunds :: String -> ClientM QueryFundsResponse
deposit' :: DepositSchema -> ClientM TxBuiltResponse
withdraw :: WithdrawSchema -> ClientM TxBuiltResponse
payMerchant :: PayMerchantSchema -> ClientM TxBuiltResponse
openHead :: ManageHeadSchema -> ClientM NoContent
closeHead :: String -> ClientM NoContent  -- Takes head ID as query parameter
getHeadState :: String -> ClientM HeadStateResponse  -- Takes head ID as query parameter

-- Generate client from API definition
(queryFunds :<|> deposit' :<|> withdraw :<|> payMerchant :<|> openHead :<|> closeHead :<|> getHeadState) = client hydraAPI

-- Exported wrapper (keeping original name for compatibility)
deposit :: DepositSchema -> ClientM TxBuiltResponse
deposit = deposit'

-- | Run a Hydra client action with logging
runHydraClient :: String -> ClientM a -> IO (Either HydraClientError a)
runHydraClient baseUrl clientAction = do
  -- Create manager with hook to capture request details
  bodyRef <- newIORef Nothing
  headersRef <- newIORef []
  
  let captureHook req = do
        -- Capture headers
        let headers = HTTP.requestHeaders req
        
        -- FIX: Remove charset=utf-8 from Content-Type header
        -- Server rejects "application/json;charset=utf-8" but accepts "application/json"
        -- Verified with curl: charset=utf-8 causes "undefined" fields, without it works
        let contentTypeHeaderName = BS8.pack "Content-Type"
            applicationJson = BS8.pack "application/json"
            fixedHeaders = map (\(k, v) -> 
              -- HeaderName is CI ByteString (case-insensitive)
              -- The show representation might be complex, try simpler approach:
              -- Check if header value contains charset=utf-8 and if k matches Content-Type
              -- by checking if lowercased string representation matches
              let kStr = show k
                  -- Extract actual header name from show output (might be "CI {original = \"Content-Type\"}")
                  kName = if "Content-Type" `isInfixOf` kStr || "content-type" `isInfixOf` (map toLower kStr)
                            then "Content-Type"
                            else kStr
                  kBytes = BS8.pack kName
                  kLower = BS8.map toLower kBytes
                  contentTypeLower = BS8.map toLower contentTypeHeaderName
                  isContentType = kLower == contentTypeLower
                  hasCharset = "charset=utf-8" `BS.isInfixOf` v
              in if isContentType && hasCharset
                then (k, applicationJson)  -- Replace value, keep original header name
                else (k, v)) headers
        
        -- Store fixed headers for debug output
        writeIORef headersRef fixedHeaders
        
        -- Capture body
        case HTTP.requestBody req of
          HTTP.RequestBodyLBS lbs -> do
            writeIORef bodyRef (Just lbs)
          HTTP.RequestBodyBS bs -> do
            let lbs = BL.fromStrict bs
            writeIORef bodyRef (Just lbs)
          _ -> return ()
        
        -- CRITICAL: Return modified request with fixed headers
        -- This removes charset=utf-8 from Content-Type, which the server requires
        return req { HTTP.requestHeaders = fixedHeaders }
  
  let managerSettings = HTTP.defaultManagerSettings { HTTP.managerModifyRequest = captureHook }
  manager <- HTTP.newManager managerSettings
  
  let scheme = if "https://" `isPrefixOf` baseUrl then Https else Http
  let clientEnv = mkClientEnv manager (BaseUrl scheme (extractHost baseUrl) (extractPort baseUrl) "")
  
  result <- try $ runClientM clientAction clientEnv
  
  -- Capture debug info
  capturedBody <- readIORef bodyRef
  capturedHeaders <- readIORef headersRef
  
  let debugInfo = case capturedBody of
        Just body -> 
          ["Servant Request Debug:", 
           "Body length: " ++ show (BL.length body),
           "Body: " ++ TL.unpack (TLE.decodeUtf8 body),
           "Headers: " ++ show capturedHeaders]
        Nothing -> 
          ["Servant Request Debug:",
           "Body: NOT CAPTURED",
           "Headers: " ++ show capturedHeaders]
  
  case result of
    Left (HTTP.HttpExceptionRequest _ e) -> 
      return $ Left $ HydraClientException $ show e
    Left (HTTP.InvalidUrlException _ e) -> 
      return $ Left $ HydraClientException $ show e
    Right (Left clientErr) -> 
      return $ Left $ HydraClientHttpError clientErr debugInfo
    Right (Right val) -> 
      return $ Right val

-- | Format HydraClientError for display with line wrapping
formatError :: HydraClientError -> [String]
formatError (HydraClientHttpError clientErr httpDebugInfo) =
  let errStr = show clientErr
      wrappedLines = wrapError errStr
  in ["HTTP Debug Info:"] ++ httpDebugInfo ++ ["", "Error:"] ++ wrappedLines
formatError (HydraClientException errStr) =
  ["Error:"] ++ wrapError errStr

-- | Helper function to wrap long error messages into multiple lines
wrapError :: String -> [String]
wrapError errStr =
  let maxLineLength = 80
      wrapLine :: String -> [String]
      wrapLine line
        | length line <= maxLineLength = [line]
        | otherwise = take maxLineLength line : wrapLine (drop maxLineLength line)
  in concatMap wrapLine (lines errStr)

-- | Extract host from URL
extractHost :: String -> String
extractHost url = 
  case dropWhile (/= '/') $ dropWhile (/= '/') url of
    ('/' : '/' : rest) -> takeWhile (/= ':') $ takeWhile (/= '/') rest
    _ -> "localhost"

-- | Extract port from URL
extractPort :: String -> Int
extractPort url = 
  case dropWhile (/= '/') $ dropWhile (/= '/') url of
    ('/' : '/' : rest) -> 
      let afterHost = dropWhile (/= ':') $ takeWhile (/= '/') rest
      in case afterHost of
        ':' : portStr -> read $ takeWhile (/= '/') portStr
        _ -> if "https://" `isPrefixOf` url then 443 else 80  -- Default ports
    _ -> if "https://" `isPrefixOf` url then 443 else 80


