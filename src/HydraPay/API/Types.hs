{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module HydraPay.API.Types
  ( -- * Core Types
    TxOutRef(..)
  , DepositSchema(..)
  , WithdrawSchema(..)
  , PayMerchantSchema(..)
  , ManageHeadSchema(..)
  , QueryFundsResponse(..)
  , TxBuiltResponse(..)
  , FundsUtxo(..)
  
  -- * Error Types
  , HydraApiError(..)
  , AddressNotFoundError(..)
  , FundsUTxONotFoundError(..)
  , BadRequest(..)
  , InternalServerError(..)
  ) where

import Data.Aeson
import Data.Aeson.Types ()
import Data.Text (Text)
import Data.Vector (Vector)
import GHC.Generics
import Deriving.Aeson ()
import Control.Applicative ((<|>))

-- | Transaction output reference
data TxOutRef = TxOutRef
  { txOutRefHash :: Text
  , txOutRefIndex :: Int
  } deriving (Eq, Show, Generic)

instance ToJSON TxOutRef where
  toJSON = genericToJSON defaultOptions
    { fieldLabelModifier = camelTo2 '_' . drop 8  -- drop "txOutRef"
    }

instance FromJSON TxOutRef where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = camelTo2 '_' . drop 8
    }

-- | Funds UTxO with signature
data FundsUtxo = FundsUtxo
  { fundsUtxoSignature :: Maybe Text
  , fundsUtxoRef :: TxOutRef
  } deriving (Eq, Show, Generic)

instance ToJSON FundsUtxo where
  toJSON = genericToJSON defaultOptions
    { fieldLabelModifier = camelTo2 '_' . drop 9  -- drop "fundsUtxo"
    }

instance FromJSON FundsUtxo where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = camelTo2 '_' . drop 9
    }

-- | Deposit request schema
data DepositSchema = DepositSchema
  { depositUserAddress :: Text
  , depositPublicKey :: Text
  , depositAmount :: Vector (Vector Text)
  , depositFundsUtxoRef :: Maybe TxOutRef
  } deriving (Eq, Show, Generic)

instance ToJSON DepositSchema where
  toJSON = genericToJSON defaultOptions
    { fieldLabelModifier = camelTo2 '_' . drop 7  -- drop "deposit"
    }

instance FromJSON DepositSchema where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = camelTo2 '_' . drop 7
    }

-- | Withdraw request schema
data WithdrawSchema = WithdrawSchema
  { withdrawAddress :: Text
  , withdrawOwner :: Text  -- "user" or "merchant"
  , withdrawFundsUtxos :: Vector FundsUtxo
  , withdrawNetworkLayer :: Text  -- "L1" or "L2"
  } deriving (Eq, Show, Generic)

instance ToJSON WithdrawSchema where
  toJSON = genericToJSON defaultOptions
    { fieldLabelModifier = camelTo2 '_' . drop 8  -- drop "withdraw"
    }

instance FromJSON WithdrawSchema where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = camelTo2 '_' . drop 8
    }

-- | Pay merchant request schema
data PayMerchantSchema = PayMerchantSchema
  { payMerchantMerchantAddress :: Text
  , payMerchantFundsUtxoRef :: TxOutRef
  , payMerchantAmount :: Vector (Vector Text)
  , payMerchantSignature :: Text
  , payMerchantMerchantFundsUtxo :: Maybe TxOutRef
  } deriving (Eq, Show, Generic)

instance ToJSON PayMerchantSchema where
  toJSON = genericToJSON defaultOptions
    { fieldLabelModifier = camelTo2 '_' . drop 12  -- drop "payMerchant"
    }

instance FromJSON PayMerchantSchema where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = camelTo2 '_' . drop 12
    }

-- | Manage head request schema
newtype ManageHeadSchema = ManageHeadSchema
  { manageHeadPeerApiUrls :: Vector Text
  } deriving (Eq, Show, Generic)

instance ToJSON ManageHeadSchema where
  toJSON = genericToJSON defaultOptions
    { fieldLabelModifier = camelTo2 '_' . drop 11  -- drop "manageHead"
    }

instance FromJSON ManageHeadSchema where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = camelTo2 '_' . drop 11
    }

-- | Query funds response
data QueryFundsResponse = QueryFundsResponse
  { queryFundsFundsInL1 :: Vector TxOutRef
  , queryFundsFundsInL2 :: Vector TxOutRef
  , queryFundsTotalInL1 :: Value  -- Generic JSON object
  , queryFundsTotalInL2 :: Value  -- Generic JSON object
  } deriving (Eq, Show, Generic)

instance ToJSON QueryFundsResponse where
  toJSON = genericToJSON defaultOptions
    { fieldLabelModifier = camelTo2 '_' . drop 10  -- drop "queryFunds"
    }

instance FromJSON QueryFundsResponse where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = \s -> case s of
        "queryFundsFundsInL1" -> "fundsInL1"
        "queryFundsFundsInL2" -> "fundsInL2" 
        "queryFundsTotalInL1" -> "totalInL1"
        "queryFundsTotalInL2" -> "totalInL2"
        _ -> camelTo2 '_' $ drop 10 s  -- drop "queryFunds"
    }

-- | Transaction built response
data TxBuiltResponse = TxBuiltResponse
  { txBuiltCborHex :: Text
  , txBuiltFundsUtxoRef :: TxOutRef
  } deriving (Eq, Show, Generic)

instance ToJSON TxBuiltResponse where
  toJSON = genericToJSON defaultOptions
    { fieldLabelModifier = camelTo2 '_' . drop 7  -- drop "txBuilt"
    }

instance FromJSON TxBuiltResponse where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = camelTo2 '_' . drop 7
    }

-- | Address not found error
newtype AddressNotFoundError = AddressNotFoundError
  { addressNotFoundMessage :: Text
  } deriving (Eq, Show, Generic)

instance ToJSON AddressNotFoundError where
  toJSON = genericToJSON defaultOptions
    { fieldLabelModifier = camelTo2 '_' . drop 15  -- drop "addressNotFound"
    }

instance FromJSON AddressNotFoundError where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = camelTo2 '_' . drop 15
    }

-- | Funds UTxO not found error
newtype FundsUTxONotFoundError = FundsUTxONotFoundError
  { fundsUTxONotFoundMessage :: Text
  } deriving (Eq, Show, Generic)

instance ToJSON FundsUTxONotFoundError where
  toJSON = genericToJSON defaultOptions
    { fieldLabelModifier = camelTo2 '_' . drop 19  -- drop "fundsUTxONotFound"
    }

instance FromJSON FundsUTxONotFoundError where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = camelTo2 '_' . drop 19
    }

-- | Bad request error
newtype BadRequest = BadRequest
  { badRequestMessage :: Text
  } deriving (Eq, Show, Generic)

instance ToJSON BadRequest where
  toJSON = genericToJSON defaultOptions
    { fieldLabelModifier = camelTo2 '_' . drop 10  -- drop "badRequest"
    }

instance FromJSON BadRequest where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = camelTo2 '_' . drop 10
    }

-- | Internal server error
newtype InternalServerError = InternalServerError
  { internalServerErrorMessage :: Text
  } deriving (Eq, Show, Generic)

instance ToJSON InternalServerError where
  toJSON = genericToJSON defaultOptions
    { fieldLabelModifier = camelTo2 '_' . drop 20  -- drop "internalServerError"
    }

instance FromJSON InternalServerError where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = camelTo2 '_' . drop 20
    }

-- | Custom error type wrapping all API errors
data HydraApiError
  = HydraAddressNotFound AddressNotFoundError
  | HydraFundsUTxONotFound FundsUTxONotFoundError
  | HydraBadRequest BadRequest
  | HydraInternalServerError InternalServerError
  | HydraClientError Text  -- For client-side errors
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
    HydraAddressNotFound <$> parseJSON v <|>
    HydraFundsUTxONotFound <$> parseJSON v <|>
    HydraBadRequest <$> parseJSON v <|>
    HydraInternalServerError <$> parseJSON v <|>
    HydraClientError <$> (obj .: "message")
  parseJSON _ = fail "Expected JSON object"
