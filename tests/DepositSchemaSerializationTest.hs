module DepositSchemaSerializationTest (tests) where

import Data.Aeson (encode, toJSON)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import Data.List (isInfixOf)
import HydraPay.API.Types

tests :: IO ()
tests = do
  putStrLn "Testing DepositSchema serialization..."
  
  -- Create a DepositSchema matching the curl example
  let userAddress = Just $ T.pack "addr_test1qpdmd0003tt0j6h3hgaqd5xc8pydfv7auf2cmyvek4mfgtd3dzr5pcgc37ym773c8w6q5uem08ts6qskrvc6mw6nvf4s3ah3ll"
  let publicKey = Nothing
  let amountList = [(T.pack "lovelace", 100000000)]
  let fundsUtxoRef = Nothing
  
  let depositReq = DepositSchema
        { depositUserAddress = userAddress
        , depositPublicKey = publicKey
        , depositAmount = amountList
        , depositFundsUtxoRef = fundsUtxoRef
        }
  
  -- Serialize to JSON using both encodePretty (for display) and encode (what Servant uses)
  let json = toJSON depositReq
  let jsonPrettyStr = TL.unpack $ TLE.decodeUtf8 $ encodePretty json
  let jsonCompactStr = TL.unpack $ TLE.decodeUtf8 $ encode depositReq
  
  putStrLn "\n=== Serialized JSON (Pretty) ==="
  putStrLn jsonPrettyStr
  putStrLn "\n=== Serialized JSON (Compact, what Servant uses) ==="
  putStrLn jsonCompactStr
  
  -- Use compact version for verification (what Servant actually sends)
  let jsonStr = jsonCompactStr
  
  -- Verify structure
  putStrLn "\n=== Verification ==="
  let hasUserAddress = "user_address" `isInfixOf` jsonStr
  let hasPublicKey = "publicKey" `isInfixOf` jsonStr
  let hasAmount = "\"amount\"" `isInfixOf` jsonStr && "lovelace" `isInfixOf` jsonStr && "100000000" `isInfixOf` jsonStr
  let hasFundsUtxoRef = "fundsUtxoRef" `isInfixOf` jsonStr
  let hasNullPublicKey = "publicKey" `isInfixOf` jsonStr && "null" `isInfixOf` jsonStr
  let hasNullFundsUtxoRef = "fundsUtxoRef" `isInfixOf` jsonStr && "null" `isInfixOf` jsonStr
  
  putStrLn $ "✓ user_address present: " ++ show hasUserAddress
  putStrLn $ "✓ publicKey present: " ++ show hasPublicKey
  putStrLn $ "✓ amount present with correct values: " ++ show hasAmount
  putStrLn $ "✓ fundsUtxoRef present: " ++ show hasFundsUtxoRef
  putStrLn $ "✓ publicKey is null: " ++ show hasNullPublicKey
  putStrLn $ "✓ fundsUtxoRef is null: " ++ show hasNullFundsUtxoRef
  
  -- Check that amount is an array with number (not string)
  let hasQuotedNumber = "\"100000000\"" `isInfixOf` jsonStr
  let hasUnquotedNumber = "\"lovelace\",\n            100000000" `isInfixOf` jsonStr ||
                          "\"lovelace\", 100000000" `isInfixOf` jsonStr ||
                          ("lovelace" `isInfixOf` jsonStr && "100000000" `isInfixOf` jsonStr && not hasQuotedNumber)
  let amountIsNumber = hasUnquotedNumber
  
  putStrLn $ "✓ amount value is number (not string): " ++ show amountIsNumber
  putStrLn $ "✓ amount format is [[\"lovelace\", 100000000]]: " ++ show (hasAmount && amountIsNumber)
  
  if hasUserAddress && hasPublicKey && hasAmount && hasFundsUtxoRef && hasNullPublicKey && hasNullFundsUtxoRef && amountIsNumber
    then do
      putStrLn "\n✅ All tests passed! Serialization is correct."
      return ()
    else do
      putStrLn "\n❌ Some tests failed!"
      fail "Serialization verification failed"
  