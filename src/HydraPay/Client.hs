{-# LANGUAGE OverloadedStrings #-}

module HydraPay.Client
  ( -- * Client functions
    queryFunds
  , deposit
  , withdraw
  , payMerchant
  , openHead
  , closeHead
    
  -- * Client runner
  , runHydraClient
  , HydraClientError(..)
    
  -- * Re-exports
  , module HydraPay.API.Types
  ) where

import Control.Exception (try)
import Data.List (isPrefixOf)
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Servant.Client
import Servant.Client.Core (ClientError(..))
import Servant (NoContent)
import Servant.API

import HydraPay.API
import HydraPay.API.Types

-- | Client error type
data HydraClientError
  = HydraClientHttpError ClientError
  | HydraClientException String
  deriving (Eq, Show)

-- | Generated client functions
queryFunds :: String -> ClientM QueryFundsResponse
deposit :: DepositSchema -> ClientM TxBuiltResponse
withdraw :: WithdrawSchema -> ClientM TxBuiltResponse
payMerchant :: PayMerchantSchema -> ClientM TxBuiltResponse
openHead :: ManageHeadSchema -> ClientM NoContent
closeHead :: ManageHeadSchema -> ClientM NoContent

(queryFunds :<|> deposit :<|> withdraw :<|> payMerchant :<|> openHead :<|> closeHead) = client hydraAPI

-- | Run a Hydra client action with logging
runHydraClient :: String -> ClientM a -> IO (Either HydraClientError a)
runHydraClient baseUrl clientAction = do
  manager <- newTlsManager
  let scheme = if "https://" `isPrefixOf` baseUrl then Https else Http
  let clientEnv = mkClientEnv manager (BaseUrl scheme (extractHost baseUrl) (extractPort baseUrl) "")
  
  result <- try $ runClientM clientAction clientEnv
  case result of
    Left (HttpExceptionRequest _ e) -> 
      return $ Left $ HydraClientException $ show e
    Left (InvalidUrlException _ e) -> 
      return $ Left $ HydraClientException $ show e
    Right (Left clientErr) -> 
      return $ Left $ HydraClientHttpError clientErr
    Right (Right val) -> 
      return $ Right val

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
