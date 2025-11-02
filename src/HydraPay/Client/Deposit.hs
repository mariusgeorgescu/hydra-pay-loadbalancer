{-# LANGUAGE OverloadedStrings #-}

module HydraPay.Client.Deposit
  ( execDeposit
  ) where

import qualified Data.Text as Text
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Aeson.Encode.Pretty (encodePretty)
import HydraPay.API.Types
import HydraPay.Client (runHydraClient, deposit, formatError)

-- | Execute deposit - same logic as TUI but in a shared module
execDeposit :: String -> String -> String -> String -> String -> IO [String]
execDeposit baseUrl userAddress publicKey assetUnit amount = do
  let amountList = [(Text.pack assetUnit, read amount :: Integer)]
  let userAddrMaybe = if null userAddress then Nothing else Just $ Text.pack userAddress
  let pubKeyMaybe = if null publicKey then Nothing else Just $ Text.pack publicKey
  let depositReq =
        DepositSchema
          { depositUserAddress = userAddrMaybe
          , depositPublicKey = pubKeyMaybe
          , depositAmount = amountList
          , depositFundsUtxoRef = Nothing
          }
  result <- runHydraClient baseUrl $ deposit depositReq
  
  case result of
    Right tx -> 
      return ["Success!", "", TL.unpack $ TLE.decodeUtf8 $ encodePretty tx]
    Left err -> 
      return $ formatError err

