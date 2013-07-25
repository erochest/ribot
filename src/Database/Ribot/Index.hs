{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

-- This indexes messages into the database's inverse index.

module Database.Ribot.Index
    ( indexItem
    , indexMessage
    , indexTopic
    , reindex
    , reindexMessages
    , reindexTopics
    , clearIndex
    ) where

import           Control.Monad.Logger
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
import qualified Data.Text as T
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Ribot hiding (tokenText)
import           Text.Ribot.Tokenizer (tokenize, getTokenText)


-- This takes a Message, breaks it apart, and adds the indexes into the
-- database.
indexItem :: SavedItem -> SqlPersistM ()
indexItem NothingSaved       = return ()
indexItem (SavedTopic tId)   = do
    topic' <- get tId
    case topic' of
        Just topic -> indexTopic tId topic
        Nothing    -> return ()
indexItem (SavedMessage mId) = do
    message' <- get mId
    case message' of
        Just message -> indexMessage mId message
        Nothing      -> return ()

-- This tokenizes and indexes a message.
indexMessage :: MessageId -> Message -> SqlPersistM ()
indexMessage mId message =
    index' mId "messageId" message messageText

-- This tokenizes and indexes a topic.
indexTopic :: TopicId -> Topic -> SqlPersistM ()
indexTopic tId topic =
    index' tId "topicId" topic topicText

-- This actually handles inserting the tokens into the database.
index' :: (PersistEntity val)
       => Key val               -- the database ID
       -> T.Text                -- the ID column
       -> val                   -- the database item
       -> (val -> T.Text)       -- the text getter function
       -> SqlPersistM ()
index' dbId idCol item getText =
    makeIndex $ tokenizeItem (dbId, item) getText
    where makeIndex tokens = do
            addTempTable
            loadMessageTokens id' tokens
            forM_ sqls $ \sql -> rawExecute sql [id']
            transactionSave

          id' = toPersistValue dbId

          sqls :: [T.Text]
          sqls = [ " INSERT OR IGNORE INTO \"Token\" (text) \
                   \ SELECT text FROM msg_token \
                   \ WHERE \"messageId\"=?; "
                 , " INSERT OR REPLACE INTO msg_token \
                   \ (\"tokenId\", \"messageId\", text) \
                   \ SELECT t.id, mt.\"messageId\", t.text \
                   \ FROM \"Token\" t \
                   \ JOIN msg_token mt ON mt.text=t.text \
                   \ WHERE mt.\"messageId\"=?; "
                , T.concat [ " INSERT INTO \"Position\" \
                             \ (\"tokenId\", \"", idCol, "\", \"", otherColumn idCol, "\") \
                             \ SELECT \"tokenId\", \"messageId\", NULL \
                             \ FROM msg_token \
                             \ WHERE \"messageId\"=?; "
                           ]
                , " DELETE FROM msg_token WHERE \"messageId\"=?; "
                ]

loadMessageTokens :: PersistValue -> [T.Text] -> SqlPersistM ()
loadMessageTokens id' tokens =
    mapM_ (rawExecute sql) [ [id', PersistText token] | token <- tokens ]
    where sql = " INSERT OR IGNORE INTO msg_token \
                \ (\"messageId\", text) VALUES (?, ?); "

-- This returns the column for the item type not being indexed.
otherColumn :: T.Text -> T.Text
otherColumn "messageId" = "topicId"
otherColumn "topicId"   = "messageId"
otherColumn x           = x

-- A more generic tokenizing function.
tokenizeItem :: PersistEntity val => (Key val, val) -> (val -> T.Text) -> [T.Text]
tokenizeItem (id', item) getText =
    getTokenText . tokenize (show id') $ getText item

-- This reindexes everything.
reindex :: SqlPersistM ()
reindex =  addTempTable
        >> clearIndex
        >> reindexMessages
        >> reindexTopics
        >> transactionSave

clearIndex :: ( MonadIO m
              , MonadUnsafeIO m
              , MonadThrow m
              , MonadLogger m
              , MonadBaseControl IO m
              , MonadResource m
              )
           => SqlPersistT m ()
clearIndex = do
    deleteWhere ([] :: [Filter Position])
    deleteWhere ([] :: [Filter Database.Ribot.Token])
    clearWorkingTable

clearWorkingTable :: (MonadIO m, MonadLogger m) => SqlPersistT m ()
clearWorkingTable = rawExecute "DELETE FROM msg_token;" []

reindexMessages :: SqlPersistM ()
reindexMessages = do
    messages <- selectList ([] :: [Filter Message]) []
    forM_ messages $ \(Entity mId message) -> index' mId "messageId" message messageText

reindexTopics :: SqlPersistM ()
reindexTopics = do
    topics <- selectList ([] :: [Filter Topic]) []
    forM_ topics $ \(Entity tId topic) -> index' tId "topicId" topic topicText

