{-# LANGUAGE DataKinds #-}

module HydraPay.API
  ( HydraAPI
  , hydraAPI
  , UserAPI
  , userAPI
  , AdminAPI
  , adminAPI
  ) where

import Servant
import HydraPay.API.Types

-- | User-facing API endpoints
type UserAPI =
  "query-funds" :> QueryParam' '[Required] "address" String :> Get '[JSON] QueryFundsResponse
  :<|> "deposit" :> ReqBody '[JSON] DepositSchema :> Post '[JSON] TxBuiltResponse
  :<|> "withdraw" :> ReqBody '[JSON] WithdrawSchema :> Post '[JSON] TxBuiltResponse
  :<|> "pay-merchant" :> ReqBody '[JSON] PayMerchantSchema :> Post '[JSON] TxBuiltResponse

-- | Admin API endpoints
type AdminAPI =
  "open-head" :> ReqBody '[JSON] ManageHeadSchema :> Post '[JSON] OperationResponse
  :<|> "close-head" :> QueryParam' '[Required] "id" String :> Post '[JSON] HeadStateResponse
  :<|> "get-head" :> Get '[JSON] (Maybe String)
  :<|> "state" :> QueryParam' '[Required] "id" String :> Get '[JSON] HeadStateResponse
  :<|> "get-uncommitted-deposits" :> Get '[JSON] UncommittedDepositsResponse

-- | The complete Hydra Protocol API type definition (UserAPI + AdminAPI)
-- Note: We expand both APIs into a single linear API type
type HydraAPI =
  "query-funds" :> QueryParam' '[Required] "address" String :> Get '[JSON] QueryFundsResponse
  :<|> "deposit" :> ReqBody '[JSON] DepositSchema :> Post '[JSON] TxBuiltResponse
  :<|> "withdraw" :> ReqBody '[JSON] WithdrawSchema :> Post '[JSON] TxBuiltResponse
  :<|> "pay-merchant" :> ReqBody '[JSON] PayMerchantSchema :> Post '[JSON] TxBuiltResponse
  :<|> "open-head" :> ReqBody '[JSON] ManageHeadSchema :> Post '[JSON] OperationResponse
  :<|> "close-head" :> QueryParam' '[Required] "id" String :> Post '[JSON] HeadStateResponse
  :<|> "get-head" :> Get '[JSON] (Maybe String)
  :<|> "state" :> QueryParam' '[Required] "id" String :> Get '[JSON] HeadStateResponse
  :<|> "get-uncommitted-deposits" :> Get '[JSON] UncommittedDepositsResponse

-- | Proxy for the User API type
userAPI :: Proxy UserAPI
userAPI = Proxy

-- | Proxy for the Admin API type
adminAPI :: Proxy AdminAPI
adminAPI = Proxy

-- | Proxy for the complete API type
hydraAPI :: Proxy HydraAPI
hydraAPI = Proxy

