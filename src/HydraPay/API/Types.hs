{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module HydraPay.API.Types
  ( -- * Core Types
    TxOutRef (..),
    DepositSchema (..),
    WithdrawSchema (..),
    PayMerchantSchema (..),
    ManageHeadSchema (..),
    QueryFundsResponse (..),
    TxBuiltResponse (..),
    HeadStateResponse (..),
    FundsUtxo (..),

    -- * Error Types
    HydraApiError (..),
    AddressNotFoundError (..),
    FundsUTxONotFoundError (..),
    BadRequest (..),
    InternalServerError (..),
    
    -- * Instances
    -- Explicitly re-export ToJSON instance for DepositSchema to ensure Servant can see it
  )
where

import Control.Applicative ((<|>))
import Data.Maybe (mapMaybe)
import Data.Aeson (Value(Array, String, Null, Object, Number), ToJSON(toJSON), FromJSON(parseJSON), object, (.=), (.:))
import Data.Aeson.Types (defaultOptions, genericToJSON, genericParseJSON, Options(..))
import Data.Char (toLower, toUpper, isUpper)
import Data.Scientific (Scientific)
import qualified Data.Scientific as Sci
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import GHC.Generics

-- | Convert camelCase to snake_case
camelTo2 :: Char -> String -> String
camelTo2 _ [] = []
camelTo2 separator (x:xs) = toLower x : go xs
  where
    go [] = []
    go (y:ys)
      | isUpper y = separator : toLower y : go ys
      | otherwise = y : go ys

-- | Transaction output reference
data TxOutRef = TxOutRef
  { txOutRefHash :: Text,
    txOutRefIndex :: Int
  }
  deriving (Eq, Show, Generic)

instance ToJSON TxOutRef where
  toJSON =
    genericToJSON
      defaultOptions
        { fieldLabelModifier = \s -> case s of
            "txOutRefHash" -> "txHash"
            "txOutRefIndex" -> "outputIndex"
            _ -> camelTo2 '_' $ drop 8 s -- drop "txOutRef"
        }

instance FromJSON TxOutRef where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier = \s -> case s of
            "txOutRefHash" -> "txHash"
            "txOutRefIndex" -> "outputIndex"
            _ -> camelTo2 '_' $ drop 8 s -- drop "txOutRef"
        }

-- | Funds UTxO with signature
data FundsUtxo = FundsUtxo
  { fundsUtxoSignature :: Maybe Text,
    fundsUtxoRef :: TxOutRef
  }
  deriving (Eq, Show, Generic)

instance ToJSON FundsUtxo where
  toJSON =
    genericToJSON
      defaultOptions
        { fieldLabelModifier = camelTo2 '_' . drop 9 -- drop "fundsUtxo"
        }

instance FromJSON FundsUtxo where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier = camelTo2 '_' . drop 9
        }

-- | Deposit request schema
-- NOTE: Using [(Text, Integer)] for amount, which serializes to [["lovelace", 100000000]]
-- This avoids Servant's issues with Vector (Vector Value) and [[Value]]
data DepositSchema = DepositSchema
  { depositUserAddress :: Maybe Text,
    depositPublicKey :: Maybe Text,
    depositAmount :: [(Text, Integer)],  -- List of (asset unit, amount) tuples
    depositFundsUtxoRef :: Maybe TxOutRef
  }
  deriving (Eq, Show, Generic)

instance ToJSON DepositSchema where
  toJSON (DepositSchema userAddr pubKey amt utxoRef) =
    -- Convert [(Text, Integer)] to [[Value]] format: [["lovelace", 100000000]]
    let amountArray = Array $ Vec.fromList $ map (\(unit, val) -> Array $ Vec.fromList [String unit, Number (Sci.scientific (fromIntegral val) 0)]) amt
        result = object
          [ ("user_address", case userAddr of
              Nothing -> Null
              Just addr -> String addr)
          , ("publicKey", case pubKey of
              Nothing -> Null
              Just key -> String key)
          , ("amount", amountArray)
          , ("fundsUtxoRef", case utxoRef of
              Nothing -> Null
              Just ref -> toJSON ref)
          ]
    in result

instance FromJSON DepositSchema where
  parseJSON (Object v) = do
    -- Parse amount as [[Value]] and convert to [(Text, Integer)]
    amountArray <- v .: "amount"
    let parseAmountItem (Array vec) = case Vec.toList vec of
          [String unit, Number num] -> Just (unit, floor num)
          _ -> Nothing
        parseAmountItem _ = Nothing
        amountList = case amountArray of
          Array vec -> mapMaybe parseAmountItem (Vec.toList vec)
          _ -> []
    DepositSchema
      <$> (v .: "user_address" <|> pure Nothing)
      <*> (v .: "publicKey" <|> pure Nothing)
      <*> pure amountList
      <*> (v .: "fundsUtxoRef" <|> pure Nothing)
  parseJSON _ = fail "Expected object for DepositSchema"

-- | Withdraw request schema
data WithdrawSchema = WithdrawSchema
  { withdrawAddress :: Text,
    withdrawOwner :: Text, -- "user" or "merchant"
    withdrawFundsUtxos :: Vector FundsUtxo,
    withdrawNetworkLayer :: Text -- "L1" or "L2"
  }
  deriving (Eq, Show, Generic)

instance ToJSON WithdrawSchema where
  toJSON =
    genericToJSON
      defaultOptions
        { fieldLabelModifier = camelTo2 '_' . drop 8 -- drop "withdraw"
        }

instance FromJSON WithdrawSchema where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier = camelTo2 '_' . drop 8
        }

-- | Pay merchant request schema
-- NOTE: Using [(Text, Integer)] for amount, which serializes to [["lovelace", 100000000]]
-- This avoids Servant's issues with Vector (Vector Value) and [[Value]]
data PayMerchantSchema = PayMerchantSchema
  { payMerchantMerchantAddress :: Text,
    payMerchantFundsUtxoRef :: TxOutRef,
    payMerchantAmount :: [(Text, Integer)],  -- List of (asset unit, amount) tuples
    payMerchantSignature :: Text,
    payMerchantMerchantFundsUtxo :: Maybe TxOutRef
  }
  deriving (Eq, Show, Generic)

instance ToJSON PayMerchantSchema where
  toJSON (PayMerchantSchema merchantAddr fundsUtxoRef amt signature merchantUtxo) =
    let -- Convert [(Text, Integer)] to [[Value]] format: [["lovelace", 100000000]]
        amountArray = Array $ Vec.fromList $ map (\(unit, val) -> Array $ Vec.fromList [String unit, Number (Sci.scientific (fromIntegral val) 0)]) amt
        baseObject = object
          [ ("merchant_address", String merchantAddr)
          , ("funds_utxo_ref", toJSON fundsUtxoRef)
          , ("amount", amountArray)
          , ("signature", String signature)
          , ("merchant_funds_utxo", case merchantUtxo of
              Nothing -> Null
              Just utxo -> toJSON utxo)
          ]
    in baseObject

instance FromJSON PayMerchantSchema where
  parseJSON (Object v) = do
    -- Parse amount as [[Value]] and convert to [(Text, Integer)]
    amountArray <- v .: "amount"
    let parseAmountItem (Array vec) = case Vec.toList vec of
          [String unit, Number num] -> Just (unit, floor num)
          _ -> Nothing
        parseAmountItem _ = Nothing
        amountList = case amountArray of
          Array vec -> mapMaybe parseAmountItem (Vec.toList vec)
          _ -> []
    PayMerchantSchema
      <$> v .: "merchant_address"
      <*> v .: "funds_utxo_ref"
      <*> pure amountList
      <*> v .: "signature"
      <*> (v .: "merchant_funds_utxo" <|> pure Nothing)
  parseJSON _ = fail "Expected object for PayMerchantSchema"

-- | Manage head request schema
newtype ManageHeadSchema = ManageHeadSchema
  { manageHeadPeerApiUrls :: Vector Text
  }
  deriving (Eq, Show, Generic)

instance ToJSON ManageHeadSchema where
  toJSON =
    genericToJSON
      defaultOptions
        { fieldLabelModifier = camelTo2 '_' . drop 10 -- drop "manageHead" (10 chars) -> "PeerApiUrls" -> "peer_api_urls"
        }

instance FromJSON ManageHeadSchema where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier = camelTo2 '_' . drop 10 -- drop "manageHead" (10 chars) -> "PeerApiUrls" -> "peer_api_urls"
        }

-- | Query funds response
data QueryFundsResponse = QueryFundsResponse
  { queryFundsFundsInL1 :: Vector TxOutRef,
    queryFundsFundsInL2 :: Vector TxOutRef,
    queryFundsTotalInL1 :: Value, -- Generic JSON object
    queryFundsTotalInL2 :: Value -- Generic JSON object
  }
  deriving (Eq, Show, Generic)

instance ToJSON QueryFundsResponse where
  toJSON =
    genericToJSON
      defaultOptions
        { fieldLabelModifier = camelTo2 '_' . drop 10 -- drop "queryFunds"
        }

instance FromJSON QueryFundsResponse where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier = \s -> case s of
            "queryFundsFundsInL1" -> "fundsInL1"
            "queryFundsFundsInL2" -> "fundsInL2"
            "queryFundsTotalInL1" -> "totalInL1"
            "queryFundsTotalInL2" -> "totalInL2"
            _ -> camelTo2 '_' $ drop 10 s -- drop "queryFunds"
        }

-- | Head state response
data HeadStateResponse = HeadStateResponse
  { headStateStatus :: Text
  }
  deriving (Eq, Show, Generic)

instance ToJSON HeadStateResponse where
  toJSON =
    genericToJSON
      defaultOptions
        { fieldLabelModifier = camelTo2 '_' . drop 10 -- drop "headState" -> "status"
        }

instance FromJSON HeadStateResponse where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier = \s -> case s of
            "headStateStatus" -> "status"
            _ -> camelTo2 '_' $ drop 10 s -- drop "headState"
        }

-- | Transaction built response
data TxBuiltResponse = TxBuiltResponse
  { txBuiltCborHex :: Text,
    txBuiltFundsUtxoRef :: TxOutRef
  }
  deriving (Eq, Show, Generic)

instance ToJSON TxBuiltResponse where
  toJSON =
    genericToJSON
      defaultOptions
        { fieldLabelModifier = camelTo2 '_' . drop 7 -- drop "txBuilt"
        }

instance FromJSON TxBuiltResponse where
  parseJSON (Object v) = TxBuiltResponse
    <$> v .: "cborHex"
    <*> v .: "fundsUtxoRef"
  parseJSON _ = fail "Expected object for TxBuiltResponse"

-- | Address not found error
newtype AddressNotFoundError = AddressNotFoundError
  { addressNotFoundMessage :: Text
  }
  deriving (Eq, Show, Generic)

instance ToJSON AddressNotFoundError where
  toJSON =
    genericToJSON
      defaultOptions
        { fieldLabelModifier = camelTo2 '_' . drop 15 -- drop "addressNotFound"
        }

instance FromJSON AddressNotFoundError where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier = camelTo2 '_' . drop 15
        }

-- | Funds UTxO not found error
newtype FundsUTxONotFoundError = FundsUTxONotFoundError
  { fundsUTxONotFoundMessage :: Text
  }
  deriving (Eq, Show, Generic)

instance ToJSON FundsUTxONotFoundError where
  toJSON =
    genericToJSON
      defaultOptions
        { fieldLabelModifier = camelTo2 '_' . drop 19 -- drop "fundsUTxONotFound"
        }

instance FromJSON FundsUTxONotFoundError where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier = camelTo2 '_' . drop 19
        }

-- | Bad request error
newtype BadRequest = BadRequest
  { badRequestMessage :: Text
  }
  deriving (Eq, Show, Generic)

instance ToJSON BadRequest where
  toJSON =
    genericToJSON
      defaultOptions
        { fieldLabelModifier = camelTo2 '_' . drop 10 -- drop "badRequest"
        }

instance FromJSON BadRequest where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier = camelTo2 '_' . drop 10
        }

-- | Internal server error
newtype InternalServerError = InternalServerError
  { internalServerErrorMessage :: Text
  }
  deriving (Eq, Show, Generic)

instance ToJSON InternalServerError where
  toJSON =
    genericToJSON
      defaultOptions
        { fieldLabelModifier = camelTo2 '_' . drop 20 -- drop "internalServerError"
        }

instance FromJSON InternalServerError where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier = camelTo2 '_' . drop 20
        }

-- | Custom error type wrapping all API errors
data HydraApiError
  = HydraAddressNotFound AddressNotFoundError
  | HydraFundsUTxONotFound FundsUTxONotFoundError
  | HydraBadRequest BadRequest
  | HydraInternalServerError InternalServerError
  | HydraClientError Text -- For client-side errors
  deriving (Eq, Show)

instance ToJSON HydraApiError where
  toJSON = \case
    HydraAddressNotFound err -> toJSON err
    HydraFundsUTxONotFound err -> toJSON err
    HydraBadRequest err -> toJSON err
    HydraInternalServerError err -> toJSON err
    HydraClientError msg -> object ["message" .= msg]

instance FromJSON HydraApiError where
  parseJSON v@(Object obj) =
    HydraAddressNotFound <$> parseJSON v
      <|> HydraFundsUTxONotFound <$> parseJSON v
      <|> HydraBadRequest <$> parseJSON v
      <|> HydraInternalServerError <$> parseJSON v
      <|> HydraClientError <$> (obj .: "message")
  parseJSON _ = fail "Expected JSON object"
