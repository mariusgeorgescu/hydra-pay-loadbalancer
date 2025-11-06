{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module HydraPay.Database
  ( CommittedDeposit (..)
  , CommittedDepositId
  , initDatabase
  , insertCommittedDeposits
  , deleteDepositsByServicePort
  , getDepositsByServicePort
  , runDB
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (runStdoutLoggingT, runNoLoggingT)
import Data.List (nub)
import Database.Persist
import Database.Persist.Sql (SqlPersistT)
import Database.Persist.Sqlite (SqlBackend, createSqlitePool, runSqlPool, runMigration)
import Database.Persist.TH
import Database.Persist.Sqlite qualified as Sqlite
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text, pack)
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as Vec
import HydraPay.API.Types (UncommittedDeposit (..), TxOutRef (..))
import System.IO
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import Control.Exception (bracket)

-- | Database schema for committed deposits
share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
CommittedDeposit
    address Text
    value Text  -- JSON-encoded amount list: [["lovelace", 100000000]]
    servicePort Int  -- Service port instead of head ID (port is fixed)
    fundsUtxoHash Text
    fundsUtxoIndex Int
    UniqueDeposit fundsUtxoHash fundsUtxoIndex
    deriving Show
|]

-- | Run a database action with a connection pool
-- Suppress SQL logging to avoid interfering with TUI
-- Persistent-sqlite prints debug messages directly to stdout/stderr
-- We need to redirect both stdout and stderr to /dev/null
runDB :: Sqlite.ConnectionPool -> SqlPersistT IO a -> IO a
runDB pool action = bracket suppressOutput restoreOutput $ \_ ->
  runNoLoggingT $ do
    liftIO $ runSqlPool action pool
  where
    suppressOutput = do
      originalStdout <- hDuplicate stdout
      originalStderr <- hDuplicate stderr
      devNull <- openFile "/dev/null" WriteMode
      hDuplicateTo devNull stdout
      hDuplicateTo devNull stderr
      return (originalStdout, originalStderr, devNull)
    restoreOutput (originalStdout, originalStderr, devNull) = do
      hDuplicateTo originalStdout stdout
      hDuplicateTo originalStderr stderr
      hClose devNull
      hClose originalStdout
      hClose originalStderr

-- | Initialize database and run migrations
-- Suppress logging during migrations to avoid interfering with TUI
initDatabase :: FilePath -> IO Sqlite.ConnectionPool
initDatabase dbPath = runNoLoggingT $ do
  pool <- createSqlitePool (pack dbPath) 5
  -- Suppress logging during migration - redirect stdout/stderr and use runNoLoggingT
  liftIO $ bracket suppressOutput restoreOutput $ \_ -> do
    runNoLoggingT $ runSqlPool (runMigration migrateAll) pool
  return pool
  where
    suppressOutput = do
      originalStdout <- hDuplicate stdout
      originalStderr <- hDuplicate stderr
      devNull <- openFile "/dev/null" WriteMode
      hDuplicateTo devNull stdout
      hDuplicateTo devNull stderr
      return (originalStdout, originalStderr, devNull)
    restoreOutput (originalStdout, originalStderr, devNull) = do
      hDuplicateTo originalStdout stdout
      hDuplicateTo originalStderr stderr
      hClose devNull
      hClose originalStdout
      hClose originalStderr

-- | Insert committed deposits for a service port
insertCommittedDeposits :: Int -> Vec.Vector UncommittedDeposit -> SqlPersistT IO ()
insertCommittedDeposits servicePort' deposits = do
  let depositList = Vec.toList deposits
  mapM_ (insertDeposit servicePort') depositList
  where
    insertDeposit :: Int -> UncommittedDeposit -> SqlPersistT IO ()
    insertDeposit port (UncommittedDeposit addr amt utxoRef) = do
      -- Convert amount list to JSON string representation using Aeson
      let valueJson = TE.decodeUtf8 $ BL.toStrict $ encode amt
          (hash, idx) = case utxoRef of
            Just (TxOutRef h i) -> (h, i)
            Nothing -> ("", 0)  -- Use empty/default values if utxoRef is missing
      -- Use insertUnique to avoid duplicates
      maybeDeposit <- getBy (UniqueDeposit hash idx)
      case maybeDeposit of
        Nothing -> do
          _ <- insert $ CommittedDeposit addr valueJson port hash idx
          return ()
        Just _ -> return ()  -- Already exists, skip

-- | Delete all deposits for a given service port
deleteDepositsByServicePort :: Int -> SqlPersistT IO ()
deleteDepositsByServicePort servicePort' = do
  deleteWhere [CommittedDepositServicePort ==. servicePort']

-- | Get all unique addresses for a given service port
getDepositsByServicePort :: Int -> SqlPersistT IO [Text]
getDepositsByServicePort servicePort' = do
  deposits <- selectList [CommittedDepositServicePort ==. servicePort'] []
  -- Return unique addresses
  return $ nub $ map (committedDepositAddress . entityVal) deposits

