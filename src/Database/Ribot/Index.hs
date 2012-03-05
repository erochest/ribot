{-# LANGUAGE OverloadedStrings #-}

-- This indexes messages into the database's inverse index.

module Database.Ribot.Index
    ( indexItem
    , indexMessage
    -- , indexTopic
    ) where

import           Control.Applicative ((<$>), (<*>))
import           Control.Exception (onException)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Resource (ResourceIO)
import           Data.Maybe
import qualified Data.Text as T
import           Database.Persist
import qualified Database.Persist.GenericSql.Internal as I
import           Database.Persist.GenericSql.Raw (execute, getStmt, withStmt)
import           Database.Persist.Sqlite
import           Database.Persist.Store
import           Database.Ribot hiding (tokenText)
import           Text.Bakers12.Tokenizer.Types (Token(..))
import           Text.Ribot.Tokenizer (tokenize)

import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL

-- This takes a Message, breaks it apart, and adds the indexes into the
-- database.
indexItem :: (ResourceIO m) => SavedItem -> SqlPersist m ()
indexItem NothingSaved       = return ()
indexItem (SavedTopic tId)   = return ()
indexItem (SavedMessage mId) = do
    message' <- get mId
    case message' of
        Just message -> indexMessage mId message
        Nothing      -> return ()

-- This tokenizes and indexes a message.
-- indexMessage :: ResourceIO m => MessageId -> Message -> SqlPersist m ()
indexMessage mId message =
    -- Yucky. Very unsafe. But it should only reach this if the message
    -- is in the database.
    either (\_ -> return ())
           (index' (toPersistValue mId) "messageId" . map tokenText)
           (tokenize "" . T.unpack $ messageText message)

-- This actually handles inserting the tokens into the database.
index' :: ResourceIO m => PersistValue -> T.Text -> [T.Text] -> SqlPersist m ()
index' id' idCol tokens = do
    addTempTable
    loadMessageTokens id' tokens
    updateMessageTokenTable id'
    updateMessageIds id'
    updateMessageIndex id' idCol
    cleanUp id'
    commit

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

updateMessageTokenTable :: ResourceIO m => PersistValue -> SqlPersist m ()
updateMessageTokenTable id' = execute sql [id']
    where sql = " INSERT OR IGNORE INTO \"Token\" (text) \
                \ SELECT text FROM msg_token \
                \ WHERE \"messageId\"=?; "

updateMessageIds :: ResourceIO m => PersistValue -> SqlPersist m ()
updateMessageIds id' = execute sql [id']
    where sql = " INSERT OR REPLACE INTO msg_token \
                \ (\"tokenId\", \"messageId\", text) \
                \ SELECT t.id, mt.\"messageId\", t.text \
                \ FROM \"Token\" t \
                \ JOIN msg_token mt ON mt.text=t.text \
                \ WHERE mt.\"messageId\"=?; "

updateMessageIndex :: ResourceIO m => PersistValue -> T.Text -> SqlPersist m ()
updateMessageIndex id' colName = execute sql [id']
    where sql = T.concat [ " INSERT INTO \"Position\" \
                           \ (\"tokenId\", \"", colName, "\", \"", otherColumn colName, "\") \
                           \ SELECT \"tokenId\", \"messageId\", NULL \
                           \ FROM msg_token \
                           \ WHERE \"messageId\"=?; "
                         ]

cleanUp :: ResourceIO m => PersistValue -> SqlPersist m ()
cleanUp id' = execute sql [id']
    where sql = " DELETE FROM msg_token WHERE \"messageId\"=?; "

-- This returns the column for the item type not being indexed.
otherColumn :: T.Text -> T.Text
otherColumn "messageId" = "topicId"
otherColumn "topicId"   = "messageId"

