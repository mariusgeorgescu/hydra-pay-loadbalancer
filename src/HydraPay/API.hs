{-# LANGUAGE DataKinds #-}

module HydraPay.API
  ( HydraAPI
  , hydraAPI
  ) where

import Servant
import HydraPay.API.Types

-- | The Hydra Protocol API type definition
type HydraAPI =
  "query-funds" :> QueryParam' '[Required] "address" String :> Get '[JSON] QueryFundsResponse
  :<|> "deposit" :> ReqBody '[JSON] DepositSchema :> Post '[JSON] TxBuiltResponse
  :<|> "withdraw" :> ReqBody '[JSON] WithdrawSchema :> Post '[JSON] TxBuiltResponse
  :<|> "pay-merchant" :> ReqBody '[JSON] PayMerchantSchema :> Post '[JSON] TxBuiltResponse
  :<|> "open-head" :> ReqBody '[JSON] ManageHeadSchema :> Post '[JSON] OperationResponse
  :<|> "close-head" :> QueryParam' '[Required] "id" String :> Post '[JSON] HeadStateResponse
  :<|> "get-head" :> Get '[JSON] (Maybe String)
  :<|> "state" :> QueryParam' '[Required] "id" String :> Get '[JSON] HeadStateResponse

-- | Proxy for the API type
hydraAPI :: Proxy HydraAPI
hydraAPI = Proxy

