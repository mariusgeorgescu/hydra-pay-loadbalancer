{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Network.Wai.Handler.Warp (run)
import Servant
import Servant.Server (err503, err502)
import Control.Monad.Except (throwError)
import HydraPay.API
import HydraPay.API.Types
import HydraPay.ServiceScanner (scanAvailableServices, Service, serviceBaseUrl, serviceName)
import HydraPay.Client (queryFunds, withdraw, runHydraClient, HydraClientError(..), formatError)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (TVar, newTVarIO, readTVarIO, writeTVar, atomically)
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import System.IO (hPutStrLn, stderr)
import Data.Aeson (Value(..), object)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Vector as Vec
import Data.Maybe (mapMaybe)
import qualified Data.ByteString.Lazy as BL
import Data.Text.Encoding (encodeUtf8)
import Data.Text (pack)

-- | Server state containing available services
data ServerState = ServerState
  { serverServices :: TVar [Service]
  }

-- | Server handlers (undefined for now)
server :: ServerState -> Server UserAPI
server state = 
  queryFundsHandler state
  :<|> depositHandler state
  :<|> withdrawHandler state
  :<|> payMerchantHandler state

-- | Aggregate two JSON objects by adding values with the same keys
aggregateJsonObjects :: Value -> Value -> Value
aggregateJsonObjects (Object obj1) (Object obj2) =
  let
    -- Start with obj1, then merge obj2 into it
    combined = foldl addToMap obj1 (KM.toList obj2)
    addToMap acc (key, val) =
      case KM.lookup key acc of
        Just (Number n1) | Number n2 <- val ->
          -- Both are numbers: add them
          KM.insert key (Number (n1 + n2)) acc
        Just (Object o1) | Object o2 <- val ->
          -- Both are objects: recursively aggregate
          KM.insert key (aggregateJsonObjects (Object o1) (Object o2)) acc
        Just _ ->
          -- Key exists but types don't match: keep first value
          acc
        Nothing ->
          -- Key doesn't exist: add it
          KM.insert key val acc
  in Object combined
aggregateJsonObjects val1 _ = val1  -- Fallback: return first value if not objects

-- | Query funds handler - aggregates results from all available services
queryFundsHandler :: ServerState -> String -> Handler QueryFundsResponse
queryFundsHandler state address = do
  -- Read current list of services
  services <- liftIO $ readTVarIO (serverServices state)
  
  if null services
    then do
      -- Return empty response if no services available
      return $ QueryFundsResponse
        { queryFundsFundsInL1 = Vec.empty
        , queryFundsFundsInL2 = Vec.empty
        , queryFundsTotalInL1 = object []
        , queryFundsTotalInL2 = object []
        }
    else do
      -- Query all services and collect results
      results <- liftIO $ mapM (queryServiceFunds address) services
      
      -- Filter out errors and aggregate successful results
      let successfulResults = mapMaybe id results
      
      if null successfulResults
        then do
          -- Return empty response if all queries failed
          return $ QueryFundsResponse
            { queryFundsFundsInL1 = Vec.empty
            , queryFundsFundsInL2 = Vec.empty
            , queryFundsTotalInL1 = object []
            , queryFundsTotalInL2 = object []
            }
        else do
          -- Aggregate results (only L2 values are aggregated, L1 kept from first response)
          let aggregated = foldl1 aggregateQueryFundsResponse successfulResults
          return aggregated

-- | Query funds from a single service
queryServiceFunds :: String -> Service -> IO (Maybe QueryFundsResponse)
queryServiceFunds address service = do
  let baseUrl = serviceBaseUrl service
  result <- runHydraClient baseUrl $ queryFunds address
  case result of
    Left _ -> return Nothing  -- Ignore errors, continue with other services
    Right funds -> return $ Just funds

-- | Aggregate two QueryFundsResponse values
-- Only aggregates L2 values (fundsInL2 and totalInL2)
-- L1 values are kept from the first response (or empty)
aggregateQueryFundsResponse :: QueryFundsResponse -> QueryFundsResponse -> QueryFundsResponse
aggregateQueryFundsResponse r1 r2 =
  QueryFundsResponse
    { queryFundsFundsInL1 = queryFundsFundsInL1 r1  -- Keep L1 from first response
    , queryFundsFundsInL2 = queryFundsFundsInL2 r1 Vec.++ queryFundsFundsInL2 r2  -- Aggregate L2
    , queryFundsTotalInL1 = queryFundsTotalInL1 r1  -- Keep L1 from first response
    , queryFundsTotalInL2 = aggregateJsonObjects (queryFundsTotalInL2 r1) (queryFundsTotalInL2 r2)  -- Aggregate L2
    }

-- | Deposit handler
depositHandler :: ServerState -> DepositSchema -> Handler TxBuiltResponse
depositHandler _ _ = undefined

-- | Withdraw handler - forwards request to first available service
withdrawHandler :: ServerState -> WithdrawSchema -> Handler TxBuiltResponse
withdrawHandler state withdrawReq = do
  -- Read current list of services
  services <- liftIO $ readTVarIO (serverServices state)
  
  case services of
    [] -> do
      -- No services available
      throwError err503 { errBody = BL.fromStrict $ encodeUtf8 $ pack "No services available" }
    (firstService:_) -> do
      -- Forward request to first available service
      let baseUrl = serviceBaseUrl firstService
      result <- liftIO $ runHydraClient baseUrl $ withdraw withdrawReq
      case result of
        Left err -> do
          -- Forward error from service
          let errorMsg = unlines $ formatError err
          throwError err502 { errBody = BL.fromStrict $ encodeUtf8 $ pack errorMsg }
        Right txBuilt -> return txBuilt

-- | Pay merchant handler
payMerchantHandler :: ServerState -> PayMerchantSchema -> Handler TxBuiltResponse
payMerchantHandler _ _ = undefined

-- | Update services list by scanning
updateServices :: TVar [Service] -> IO ()
updateServices servicesVar = do
  let host = "127.0.0.1"
      startPort = 3000
      endPort = startPort + 20 - 1  -- Scan 20 ports
  
  hPutStrLn stderr $ "Scanning ports " ++ show startPort ++ "-" ++ show endPort ++ " for available services..."
  availableServices <- scanAvailableServices host startPort endPort
  
  if null availableServices
    then do
      hPutStrLn stderr "No available services found"
    else do
      hPutStrLn stderr $ "Found " ++ show (length availableServices) ++ " available service(s):"
      mapM_ (\s -> hPutStrLn stderr $ "  - " ++ serviceName s ++ " (" ++ serviceBaseUrl s ++ ")") availableServices
  
  atomically $ writeTVar servicesVar availableServices

-- | Background thread that updates services every 30 seconds
startServiceUpdater :: TVar [Service] -> IO ()
startServiceUpdater servicesVar = do
  _ <- forkIO $ forever $ do
    threadDelay (30 * 1000000)  -- 30 seconds in microseconds
    updateServices servicesVar
  return ()

-- | Main entry point
main :: IO ()
main = do
  -- Initialize services list
  servicesVar <- newTVarIO []
  
  -- Initial scan
  updateServices servicesVar
  
  -- Start background updater thread
  startServiceUpdater servicesVar
  
  -- Create server state
  let state = ServerState { serverServices = servicesVar }
  
  -- Start server
  let port = 8080
  hPutStrLn stderr $ "Starting Hydra Pay Server on port " ++ show port
  hPutStrLn stderr "Service list will be updated every 30 seconds"
  run port (serve userAPI (server state))

