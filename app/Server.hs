{-# LANGUAGE DataKinds #-}

module Main (main) where

import Network.Wai.Handler.Warp (run)
import Servant
import HydraPay.API
import HydraPay.API.Types
import HydraPay.ServiceScanner (scanAvailableServices, Service, serviceBaseUrl, serviceName)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (TVar, newTVarIO, readTVarIO, writeTVar, atomically)
import Control.Monad (forever)
import System.IO (hPutStrLn, stderr)

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

-- | Query funds handler
queryFundsHandler :: ServerState -> String -> Handler QueryFundsResponse
queryFundsHandler _ _ = undefined

-- | Deposit handler
depositHandler :: ServerState -> DepositSchema -> Handler TxBuiltResponse
depositHandler _ _ = undefined

-- | Withdraw handler
withdrawHandler :: ServerState -> WithdrawSchema -> Handler TxBuiltResponse
withdrawHandler _ _ = undefined

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

