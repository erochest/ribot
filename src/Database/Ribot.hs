{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, TemplateHaskell,
             OverloadedStrings, GADTs, FlexibleContexts #-}
{-# LANGUAGE EmptyDataDecls #-}

-- This handles the database interface for Ribot. This defines the schema and
-- the interface types, and it defines the "model" functions, i.e., those that
-- have any database access.

module Database.Ribot
    ( UserGeneric(..)
    , User
    , UserId
    , Unique(..)
    , MessageGeneric(..)
    , Message
    , MessageId
    , TopicGeneric(..)
    , Topic
    , TopicId
    , TokenGeneric(..)
    , Token
    , TokenId
    , PositionGeneric(..)
    , Position
    , PositionId
    , SavedItem(..)
    , initDatabase
    , addTempTable
    , runDb
    , runPool
    , getOrCreateUser
    , getOrCreateTopic
    , saveMessage
    , setUserLogging
    , getUserMessages
    ) where

import           Database.Persist
import           Database.Persist.GenericSql.Raw (execute)
import           Database.Persist.Sqlite
import           Database.Persist.TH
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time
import           Control.Monad.Logger (MonadLogger)
import           Control.Monad.IO.Class (liftIO, MonadIO)
import           Control.Monad.Trans.Resource
import qualified Network.IRC.Base as B

-- This creates the model types from their names.
share [mkPersist sqlSettings, mkMigrate "migrateAll"] $(persistFile "config/models")

-- This is for the output of `saveMessage`. This lets me wrap up the its that
-- was saved so I can retrieve it later. This could probably be a newtype for
-- `Maybe (Either Topic Message)`, but this seems more extensible.
data SavedItem = NothingSaved
               | SavedTopic TopicId
               | SavedMessage MessageId

-- This initializes the database by opening the connection and migrating.
initDatabase :: FilePath -> IO ()
initDatabase dbFile = runDb dbFile $ do
    runMigrationSilent migrateAll
    addIndices
    addTempTable
    return ()

-- This takes a function and runs it in the context of a SQLite database.
runDb :: (MonadIO m, MonadBaseControl IO m)
      => FilePath -> SqlPersist m a -> m a
runDb sqliteFile = withSqliteConn (T.pack sqliteFile) . runSqlConn

-- This takes a function and runs it in the context of a pool of SQLite
-- database connections.
runPool :: (MonadIO m, MonadBaseControl IO m)
        => FilePath -> Int -> SqlPersist m a -> m a
runPool sqliteFile poolSize =
    withSqlitePool (T.pack sqliteFile) poolSize . runSqlPool

-- This takes a database and executes the SQL to create the database's
-- indices. These include "IF NOT EXISTS" phrases, so this can safely be
-- executed more than once on the same database.
addIndices :: (MonadIO m, MonadLogger m) => SqlPersist m ()
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
--
-- This might be unsafe in some circumstances. That is, messageId could be
-- either a messageId or a topicId. This could only be a problem very, very
-- early in the indexing process, when there are very few messages and very few
-- topics, and a topic and a message with the same ID are both being indexed at
-- the same time. If you're re-indexing the entire database, this isn't an
-- issue, however; because messages and topics aren't indexed at the same time.
addTempTable :: (MonadIO m, MonadLogger m)
             => SqlPersist m ()
addTempTable = execute sql []
    where
        sql = " CREATE TEMPORARY TABLE IF NOT EXISTS msg_token \
                \ (\"tokenId\" INTEGER DEFAULT NULL, \
                \  \"messageId\" INTEGER, \
                \  text VARCHAR, \
                \  UNIQUE (\"messageId\", text) ON CONFLICT IGNORE, \
                \  FOREIGN KEY (\"messageId\") REFERENCES \"Message\"(id) \
                \ );"

-- This looks for a username in the database. If it doesn't exist, this creates
-- it.
getOrCreateUser :: ( MonadIO m
                   , MonadUnsafeIO m
                   , MonadThrow m
                   , MonadLogger m
                   , MonadBaseControl IO m
                   )
                => T.Text -> SqlPersist m (Entity User)
getOrCreateUser = get' (0 :: Int)
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

-- This looks for a topic with a given text from a user. If it doesn't exist,
-- this creates it.
getOrCreateTopic :: ( MonadIO m
                    , MonadUnsafeIO m
                    , MonadThrow m
                    , MonadLogger m
                    , MonadBaseControl IO m
                    )
                 => UserId -> T.Text -> SqlPersist m (Entity Topic)
getOrCreateTopic userId text = get' (0 :: Int)
    where
        get' 3 = fail "too many attempts"
        get' n = do
            exists <- getBy $ UniqueTopic userId text
            case exists of
                Just topic -> return topic
                Nothing    -> do
                    now <- liftIO getCurrentTime
                    insert $ Topic userId text now
                    get' (n-1)

-- This takes a `Message` from IRC and saves it to the database.
saveMessage :: ( MonadIO m
               , MonadUnsafeIO m
               , MonadThrow m
               , MonadLogger m
               , MonadBaseControl IO m
               )
            => B.Message -> SqlPersist m SavedItem
saveMessage (B.Message (Just (B.NickName _    _ _)) "PRIVMSG" [_, ""]) =
    return NothingSaved
saveMessage (B.Message (Just (B.NickName _    _ _)) "PRIVMSG" [_, ('!':_)]) =
    return NothingSaved
saveMessage (B.Message (Just (B.NickName name _ _)) "PRIVMSG" [_, message]) = do
    (Entity userId user) <- (getOrCreateUser $ T.pack name)
    if userLoggingOn user
        then insertMessage userId message
        else return NothingSaved
    where insertMessage userId msg = do
            now <- liftIO getCurrentTime
            mid <- insert $ Message userId (T.pack msg) now
            commit
            return $ SavedMessage mid
saveMessage (B.Message (Just (B.NickName name _ _)) "TOPIC"   [_, topic]) = do
    (Entity userId  _) <- getOrCreateUser $ T.pack name
    (Entity topicId _) <- getOrCreateTopic userId $ T.pack topic
    commit
    return $ SavedTopic topicId
saveMessage _  = return NothingSaved

-- This takes a userId and sets the logging for it.
setUserLogging :: ( MonadIO m
                  , MonadUnsafeIO m
                  , MonadThrow m
                  , MonadLogger m
                  , MonadBaseControl IO m
                  )
               => UserId -> Bool -> SqlPersist m ()
setUserLogging userId logging =
    update userId [UserLoggingOn =. logging] >> commit

-- This returns all the messages for the user with a given user name.
getUserMessages :: ( MonadIO m
                   , MonadUnsafeIO m
                   , MonadThrow m
                   , MonadBaseControl IO m
                   , MonadLogger m
                   )
                => T.Text
                -> SqlPersist m (Maybe [Entity Message])
getUserMessages userName = do
    user' <- getBy $ UniqueUser userName
    case user' of
        Nothing   -> return Nothing
        Just (Entity userId _) ->
            Just `fmap` selectList [MessageUserId ==. userId] []

