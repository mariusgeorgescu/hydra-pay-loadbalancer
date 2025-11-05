module Main (main) where

import TUI
import System.IO (hPutStrLn, stderr, stdout, hFlush)
import Text.Read (readMaybe)
import qualified Data.Maybe

main :: IO ()
main = do
  -- Ask user for starting port and range
  putStr "Enter starting port [default: 3000]: "
  hFlush stdout
  startPortInput <- getLine
  let startPort = Data.Maybe.fromMaybe 3000 (readMaybe startPortInput)

  putStr "Enter range (number of ports to scan) [default: 20]: "
  hFlush stdout
  rangeInput <- getLine
  let range = Data.Maybe.fromMaybe 20 (readMaybe rangeInput)

  let endPort = startPort + range - 1

  hPutStrLn stderr $ "Scanning ports " ++ show startPort ++ "-" ++ show endPort ++ " for available services..."
  -- Scan ports for available services
  availableServices <- scanAvailableServices "127.0.0.1" startPort endPort

  if null availableServices
    then do
      hPutStrLn stderr $ "No available services found on ports " ++ show startPort ++ "-" ++ show endPort
      hPutStrLn stderr "Starting TUI with empty service list..."
    else do
      hPutStrLn stderr $ "Found " ++ show (length availableServices) ++ " available service(s):"
      mapM_ (\s -> hPutStrLn stderr $ "  - " ++ serviceName s ++ " (" ++ serviceBaseUrl s ++ ")") availableServices

  runTUI availableServices