{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, TemplateHaskell,
             OverloadedStrings, GADTs, FlexibleContexts #-}

-- This handles the database interface for Ribot. This defines the schema and
-- the interface types, and it defines the "model" functions, i.e., those that
-- have any database access.

module Database.Ribot
    ( User(..)
    , initDatabase
    , runDb
    , getOrCreateUser
    , saveMessage
    ) where

import           Database.Persist
import           Database.Persist.GenericSql.Raw (execute)
import           Database.Persist.Sqlite
import           Database.Persist.Store
import           Database.Persist.TH
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time
import           Control.Monad.IO.Class (liftIO, MonadIO)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Control.Monad.Trans.Resource (ResourceIO)
import qualified Network.IRC.Base as B

-- This creates the model types from their names.
share [mkPersist sqlSettings, mkMigrate "migrateAll"] $(persistFile "config/models")

-- This initializes the database by opening the connection and migrating.
initDatabase :: FilePath -> IO ()
initDatabase dbFile = runDb dbFile $ do
    runMigrationSilent migrateAll
    addIndices
    addTempTable
    return ()

-- This takes a function and runs it in the context of a SQLite database.
runDb :: (ResourceIO m) => FilePath -> SqlPersist m a -> m a
runDb sqliteFile = withSqliteConn (T.pack sqliteFile) . runSqlConn

-- This executes some raw SQL. This function can be passed to `runDb`.
execSql :: FilePath -> String -> [PersistValue] -> IO ()
execSql sqliteFile sql params =
    runDb sqliteFile $ execute (T.pack sql) params

-- This executes a list of SQL commands, none of which take parameters.
execSqlScripts :: FilePath -> [String] -> IO ()
execSqlScripts sqliteFile sqls =
    runDb sqliteFile $ mapM_ (execute' []) sqls'
    where execute' = flip execute
          sqls'    = map T.pack sqls

-- This takes a database and executes the SQL to create the database's
-- indices. These include "IF NOT EXISTS" phrases, so this can safely be
-- executed more than once on the same database.
addIndices :: (ResourceIO m) => SqlPersist m ()
addIndices = mapM_ (execute' []) sql
    where
        execute' = flip execute
        sql = [ " CREATE INDEX IF NOT EXISTS idx_message ON \"Message\" \
                    \ (id, \"userId\", posted);"
              , " CREATE INDEX IF NOT EXISTS idx_token ON \"Token\" \
                    \ (id, text);"
              , " CREATE INDEX IF NOT EXISTS idx_position on \"Position\" \
                    \ (id, \"tokenId\", \"messageId\");"
              ]

-- This creates the temporary table used for building the inverted index.
addTempTable :: (ResourceIO m) => SqlPersist m ()
addTempTable = execute sql []
    where
        sql = " CREATE TEMPORARY TABLE IF NOT EXISTS msg_token \
                \ (\"tokenId\" INTEGER DEFAULT NULL, \
                \  \"messageId\" INTEGER, \
                \  text VARCHAR, \
                \  UNIQUE (\"messageId\", text) ON CONFLICT IGNORE, \
                \  FOREIGN KEY (\"messageId\") REFERENCES \"Message\"(id) \
                \ );"

-- This looks for a username in the database. If it doesn't exist, it creates
-- it.
getOrCreateUser :: (ResourceIO m) => T.Text -> SqlPersist m (Entity User)
getOrCreateUser username = get' 0 username
    where
        -- This attempts to insert and get the user. If it takes too many
        -- tries, just fail.
        get' 3 _ = fail "too many attempts"
        get' n name = do
            exists <- getBy $ UniqueUser name
            case exists of
                Just user -> return user
                Nothing   -> do
                    insert $ User name True
                    get' (n-1) name

-- This takes a `Message` from IRC and saves it to the database.
saveMessage :: (ResourceIO m) => B.Message -> SqlPersist m (Maybe MessageId)
saveMessage m@(B.Message (Just (B.NickName name _ _)) "PRIVMSG" [_, message]) = do
    (Entity userId user) <- getOrCreateUser $ T.pack name
    now <- liftIO getCurrentTime
    messageId <- insert $ Message userId (T.pack message) now
    commit
    return $ Just messageId
-- saveMessage (B.Message prefix "TOPIC"   params) =
saveMessage m  = do
    return Nothing


