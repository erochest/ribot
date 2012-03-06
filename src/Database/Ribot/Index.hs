{-# LANGUAGE OverloadedStrings #-}

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

import           Control.Applicative ((<$>), (<*>))
import           Control.Exception (onException)
import           Control.Monad (forM_)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Resource (ResourceIO)
import qualified Data.List as L
import           Data.Maybe
import qualified Data.Text as T
import           Database.Persist
import qualified Database.Persist.GenericSql.Internal as I
import           Database.Persist.GenericSql.Raw (execute, getStmt, withStmt)
import           Database.Persist.Sqlite
import           Database.Persist.Store
import           Database.Ribot hiding (tokenText)
import           Text.Bakers12.Tokenizer.Types (Token(..))
import           Text.Ribot.Tokenizer (tokenize, getTokenText)

import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL

-- This takes a Message, breaks it apart, and adds the indexes into the
-- database.
indexItem :: (ResourceIO m) => SavedItem -> SqlPersist m ()
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
indexMessage :: ResourceIO m => MessageId -> Message -> SqlPersist m ()
indexMessage mId message =
    index' mId "messageId" message messageText

-- This tokenizes and indexes a topic.
indexTopic :: ResourceIO m => TopicId -> Topic -> SqlPersist m ()
indexTopic tId topic =
    index' tId "topicId" topic topicText

-- This actually handles inserting the tokens into the database.
index' :: (PersistEntity val, ResourceIO m)
       => Key b val             -- the database ID
       -> T.Text                -- the ID column
       -> val                   -- the database item
       -> (val -> T.Text)       -- the text getter function
       -> SqlPersist m ()
index' id idCol item getText =
    makeIndex $ tokenizeItem (id, item) getText
    where makeIndex :: ResourceIO m => [T.Text] -> SqlPersist m ()
          makeIndex tokens = do
            addTempTable
            loadMessageTokens id' tokens
            forM_ sqls $ \sql -> execute sql [id']

          id' = toPersistValue id

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

msgTokenCount :: ResourceIO m => SqlPersist m ()
msgTokenCount = do
    liftIO $ putStrLn "msg_token COUNT"
    C.runResourceT $ withStmt "SELECT COUNT(*) FROM msg_token;" []
        C.$$ CL.mapM_ $ liftIO . print
    liftIO $ putStrLn ""

loadMessageTokens :: ResourceIO m => PersistValue -> [T.Text] -> SqlPersist m ()
loadMessageTokens id' tokens = do
    stmt <- getStmt sql
    liftIO . mapM_ (execute' stmt) $ [ [id', PersistText token] | token <- tokens ]
    where execute' stmt vals = I.execute stmt vals >> I.reset stmt
          sql = " INSERT OR IGNORE INTO msg_token \
                \ (\"messageId\", text) VALUES (?, ?); "

-- This returns the column for the item type not being indexed.
otherColumn :: T.Text -> T.Text
otherColumn "messageId" = "topicId"
otherColumn "topicId"   = "messageId"

-- A more generic tokenizing function.
tokenizeItem :: PersistEntity val => (Key b val, val) -> (val -> T.Text) -> [T.Text]
tokenizeItem (id', item) getText =
    getTokenText . tokenize (show id') $ getText item

-- This reindexes everything.
reindex :: ResourceIO m => SqlPersist m ()
reindex =  addTempTable
        >> clearIndex
        >> reindexMessages
        >> reindexTopics

clearIndex :: ResourceIO m => SqlPersist m ()
clearIndex = do
    deleteWhere ([] :: [Filter Position])
    deleteWhere ([] :: [Filter Database.Ribot.Token])
    clearWorkingTable

clearWorkingTable :: ResourceIO m => SqlPersist m ()
clearWorkingTable = execute "DELETE FROM msg_token;" []

reindexMessages :: ResourceIO m => SqlPersist m ()
reindexMessages = do
    messages <- selectList ([] :: [Filter Message]) []
    forM_ messages $ \(Entity mId message) -> index' mId "messageId" message messageText

reindexTopics :: ResourceIO m => SqlPersist m ()
reindexTopics = do
    topics <- selectList ([] :: [Filter Topic]) []
    forM_ topics $ \(Entity tId topic) -> index' tId "topicId" topic topicText

