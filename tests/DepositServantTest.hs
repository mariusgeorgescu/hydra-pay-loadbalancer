{-# LANGUAGE OverloadedStrings #-}

module DepositServantTest (runDepositTest) where

import Data.List (isInfixOf)
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text.Lazy as TL
import Data.Aeson.Encode.Pretty (encodePretty)

-- Import execDeposit from shared module
import HydraPay.Client.Deposit (execDeposit)

-- Test that Servant client works correctly for deposit
runDepositTest :: IO ()
runDepositTest = do
  putStrLn "=========================================="
  putStrLn "Testing Deposit via Servant Client"
  putStrLn "=========================================="
  
  let baseUrl = "http://127.0.0.1:3001"
      userAddress = "addr_test1qpdmd0003tt0j6h3hgaqd5xc8pydfv7auf2cmyvek4mfgtd3dzr5pcgc37ym773c8w6q5uem08ts6qskrvc6mw6nvf4s3ah3ll"
      publicKey = ""  -- Optional
      assetUnit = "lovelace"
      amount = "100000000"
  
  putStrLn $ "\nTest Parameters:"
  putStrLn $ "  Base URL: " ++ baseUrl
  putStrLn $ "  User Address: " ++ userAddress
  putStrLn $ "  Public Key: " ++ (if null publicKey then "(empty)" else publicKey)
  putStrLn $ "  Asset Unit: " ++ assetUnit
  putStrLn $ "  Amount: " ++ amount
  
  putStrLn "\nCalling execDeposit..."
  result <- execDeposit baseUrl userAddress publicKey assetUnit amount
  
  putStrLn "\nResult:"
  mapM_ putStrLn result
  
  -- Check if deposit succeeded
  let resultStr = unlines result
      success = "Success!" `isInfixOf` resultStr
      hasError = "Error:" `isInfixOf` resultStr || "HTTP Debug Info:" `isInfixOf` resultStr
  
  putStrLn "\n=========================================="
  putStrLn "Test Analysis:"
  putStrLn "=========================================="
  
  if success
    then do
      putStrLn "✅ SUCCESS: Deposit completed successfully!"
    else if hasError
      then do
        putStrLn "❌ FAILURE: Deposit failed with error"
        putStrLn "   Check the error output above for details"
      else do
        putStrLn "⚠️  UNKNOWN: Could not determine test result"
        putStrLn "   Check the output above manually"

