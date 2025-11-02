module Main (main) where

import DepositSchemaSerializationTest
import DepositServantTest

main :: IO ()
main = do
  DepositSchemaSerializationTest.tests
  putStrLn "\n"
  putStrLn "=========================================="
  putStrLn "Testing Deposit via TUI execDeposit"
  putStrLn "=========================================="
  runDepositTest

