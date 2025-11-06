module Main (main) where

import DepositSchemaSerializationTest
import DepositServantTest
import ServerIntegrationTest

main :: IO ()
main = do
  DepositSchemaSerializationTest.tests
  putStrLn "\n"
  putStrLn "=========================================="
  putStrLn "Testing Deposit via TUI execDeposit"
  putStrLn "=========================================="
  runDepositTest
  putStrLn "\n"
  putStrLn "=========================================="
  putStrLn "Server Integration Tests"
  putStrLn "=========================================="
  runServerTests

