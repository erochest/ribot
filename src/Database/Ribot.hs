{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, TemplateHaskell,
             OverloadedStrings, GADTs, FlexibleContexts #-}

-- This handles the database interface for Ribot. This defines the schema and
-- the interface types, and it defines the "model" functions, i.e., those that
-- have any database access.

module Database.Ribot
    ( User(..)
    , initDatabase
    -- , runDb
    ) where

import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time
import           Control.Monad.IO.Class (liftIO, MonadIO)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Control.Monad.Trans.Resource (ResourceIO)

-- This creates the model types from their names.
share [mkPersist sqlSettings, mkMigrate "migrateAll"] $(persistFile "config/models")

-- This initializes the database by opening the connection and migrating.
initDatabase :: FilePath -> IO ()
-- initDatabase dbFile = runDb dbFile $ \ cxn -> do
initDatabase dbFile = runDb dbFile $ do
    runMigrationSilent migrateAll
    return ()

-- This takes a function and runs it in the context of a SQLite database.
runDb :: (ResourceIO m) => FilePath -> SqlPersist m a -> m a
runDb sqliteFile = withSqliteConn (T.pack sqliteFile) . runSqlConn


