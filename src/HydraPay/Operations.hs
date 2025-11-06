{-# LANGUAGE OverloadedStrings #-}

module HydraPay.Operations
  ( -- * Execution functions
    execQueryFunds
  , execDeposit
  , execWithdraw
  , execPayMerchant
  , execOpenHead
  , execCloseHead
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON, encode)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TLE
import Data.Vector qualified as Vec
import Database.Persist.Sqlite qualified as Sqlite

import HydraPay.API.Types
  ( DepositSchema (..)
  , FundsUtxo (..)
  , HeadStateResponse (..)
  , ManageHeadSchema (..)
  , OperationResponse (..)
  , PayMerchantSchema (..)
  , QueryFundsResponse (..)
  , TxOutRef (..)
  , UncommittedDepositsResponse (..)
  , WithdrawSchema (..)
  )
import HydraPay.Client
  ( HydraClientError (..)
  , closeHead
  , deposit
  , formatError
  , getHead
  , getUncommittedDeposits
  , openHead
  , payMerchant
  , queryFunds
  , runHydraClient
  , withdraw
  )
import HydraPay.Database
  ( deleteDepositsByServicePort
  , insertCommittedDeposits
  , runDB
  )

-- | Helper to format request debug info (for non-ToJSON requests like query-funds)
formatRequestDebug :: String -> [String]
formatRequestDebug requestStr = ["Request:", requestStr, ""]

-- | Helper to format request debug info (for ToJSON requests)
formatRequestJsonDebug :: ToJSON req => req -> [String]
formatRequestJsonDebug req =
  let debugJsonPretty = TL.unpack $ TLE.decodeUtf8 $ encodePretty req
      debugJsonCompact = TL.unpack $ TLE.decodeUtf8 $ encode req
  in ["Request JSON (Compact):", debugJsonCompact, "", "Request JSON (Pretty):", debugJsonPretty, ""]

-- | Execute a client action with debug info and error formatting
-- This helper consolidates the common pattern of:
-- 1. Building a request
-- 2. Encoding it to JSON for debug output
-- 3. Running the client action
-- 4. Formatting success/error responses
execWithDebug :: (ToJSON req, ToJSON res) => String -> req -> (req -> IO (Either HydraClientError res)) -> IO [String]
execWithDebug _baseUrl req clientFn = do
  let debugInfo = formatRequestJsonDebug req
  result <- clientFn req

  case result of
    Right res ->
      return $ ["Success!", ""] ++ debugInfo ++ [TL.unpack $ TLE.decodeUtf8 $ encodePretty res]
    Left err ->
      return $ debugInfo ++ formatError err

-- | Execute query funds
execQueryFunds :: String -> String -> IO [String]
execQueryFunds baseUrl addr = do
  let debugInfo = formatRequestDebug $ "Address: " ++ addr
  result <- runHydraClient baseUrl $ queryFunds addr
  case result of
    Left err -> return $ debugInfo ++ formatError err
    Right funds -> return $ ["Success!", ""] ++ debugInfo ++ [TL.unpack $ TLE.decodeUtf8 $ encodePretty funds]

-- | Execute deposit
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
  execWithDebug baseUrl depositReq (\r -> runHydraClient baseUrl $ deposit r)

-- | Execute withdraw
execWithdraw :: String -> String -> String -> String -> String -> String -> IO [String]
execWithdraw baseUrl address owner utxoHash utxoIndexStr signature = do
  let utxo =
        FundsUtxo
          { fundsUtxoSignature = Just $ Text.pack signature
          , fundsUtxoRef =
              TxOutRef
                { txOutRefHash = Text.pack utxoHash
                , txOutRefIndex = read utxoIndexStr
                }
          }
  let withdrawReq =
        WithdrawSchema
          { withdrawAddress = Text.pack address
          , withdrawOwner = Text.pack owner
          , withdrawFundsUtxos = Vec.fromList [utxo]
          , withdrawNetworkLayer = "L1"
          }
  execWithDebug baseUrl withdrawReq (runHydraClient baseUrl . withdraw)

-- | Execute pay merchant
execPayMerchant :: String -> String -> String -> String -> String -> String -> String -> String -> String -> IO [String]
execPayMerchant baseUrl merchantAddress utxoHash utxoIndexStr assetUnit amount signature merchantUtxoHash merchantUtxoIndexStr = do
  let merchantUtxo =
        if null merchantUtxoHash
          then Nothing
          else
            Just
              TxOutRef
                { txOutRefHash = Text.pack merchantUtxoHash
                , txOutRefIndex = read merchantUtxoIndexStr
                }
  let amountList = [(Text.pack assetUnit, read amount :: Integer)]
  let payMerchantReq =
        PayMerchantSchema
          { payMerchantMerchantAddress = Text.pack merchantAddress
          , payMerchantFundsUtxoRef =
              TxOutRef
                { txOutRefHash = Text.pack utxoHash
                , txOutRefIndex = read utxoIndexStr
                }
          , payMerchantAmount = amountList
          , payMerchantSignature = Text.pack signature
          , payMerchantMerchantFundsUtxo = merchantUtxo
          }
  execWithDebug baseUrl payMerchantReq (\r -> runHydraClient baseUrl $ payMerchant r)

-- | Execute open head
execOpenHead :: Sqlite.ConnectionPool -> Int -> String -> String -> IO [String]
execOpenHead dbPool' servicePort' baseUrl urlsStr = do
  -- First, fetch uncommitted deposits and persist them
  uncommittedResult <- runHydraClient baseUrl getUncommittedDeposits
  case uncommittedResult of
    Left err -> return $ ["Error fetching uncommitted deposits:"] ++ formatError err
    Right uncommittedResp -> do
      let urls = fmap Text.pack $ words urlsStr
      let openHeadReq = ManageHeadSchema {manageHeadPeerApiUrls = Vec.fromList urls}
      let debugInfo = formatRequestJsonDebug openHeadReq

      result <- runHydraClient baseUrl $ openHead openHeadReq
      case result of
        Left err -> return $ debugInfo ++ formatError err
        Right opResp -> do
          let operationId = Text.unpack (operationResponseOperationId opResp)
          -- Fetch the actual head ID
          headIdResult <- runHydraClient baseUrl getHead
          let actualHeadId = case headIdResult of
                Right (Just headId) -> headId
                _ -> operationId  -- Fallback to operation ID
          -- Insert uncommitted deposits with the service port (not head ID)
          let deposits = uncommittedDepositsDeposits uncommittedResp
          if Vec.null deposits
            then return $ ["Success!", "Head opened successfully!", "", "Operation ID: " ++ operationId, "Head ID: " ++ actualHeadId, "", "No uncommitted deposits found.", ""] ++ debugInfo
            else do
              runDB dbPool' $ insertCommittedDeposits servicePort' deposits
              return $ ["Success!", "Head opened successfully!", "", "Operation ID: " ++ operationId, "Head ID: " ++ actualHeadId, "", "Persisted " ++ show (Vec.length deposits) ++ " uncommitted deposits.", ""] ++ debugInfo

-- | Execute close head
execCloseHead :: Sqlite.ConnectionPool -> Int -> String -> String -> IO [String]
execCloseHead dbPool' servicePort' baseUrl headId = do
  let debugInfo = formatRequestDebug $ "Head ID: " ++ headId
  result <- runHydraClient baseUrl $ closeHead headId
  case result of
    Left err -> return $ debugInfo ++ formatError err
    Right stateResp -> do
      -- Delete deposits for this service port from the database
      runDB dbPool' $ deleteDepositsByServicePort servicePort'
      return $ ["Success!", "Head closed successfully!", "", "Status: " ++ Text.unpack (headStateStatus stateResp), "", "Deleted deposits for service port " ++ show servicePort' ++ " from database.", ""] ++ debugInfo

