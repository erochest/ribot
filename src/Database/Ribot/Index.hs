{-# LANGUAGE OverloadedStrings #-}

-- This indexes messages into the database's inverse index.

module Database.Ribot.Index
    ( indexItem
    ) where

import           Control.Applicative ((<$>))
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
indexItem (SavedTopic _)     = return ()
indexItem (SavedMessage mId) = index
    where
        mId' = toPersistValue mId

        index = do
            -- Yucky. Very unsafe. But it should only reach this if the message
            -- is in the database.
            msg <- fromJust <$> get mId
            either (\_ -> return ()) index'
                   (tokenize "" . T.unpack $ messageText msg)

        index' tokens = do
            addTempTable
            loadMessageTokens $ map tokenText tokens
            updateMessageTokenTable
            updateMessageIds
            updateMessageIndex
            cleanUp
            commit

        msgTokenCount :: ResourceIO m => SqlPersist m ()
        msgTokenCount = do
            liftIO $ putStrLn "msg_token COUNT"
            C.runResourceT $ withStmt "SELECT COUNT(*) FROM msg_token;" []
                C.$$ CL.mapM_ $ liftIO . print
            liftIO $ putStrLn ""

        loadMessageTokens :: ResourceIO m => [T.Text] -> SqlPersist m ()
        loadMessageTokens tokens = do
            stmt <- getStmt sql
            liftIO . mapM_ (execute' stmt) $ [ [mId', PersistText token] | token <- tokens ]
            where execute' stmt vals = I.execute stmt vals >> I.reset stmt
                  sql = " INSERT OR IGNORE INTO msg_token \
                        \ (\"messageId\", text) VALUES (?, ?); "

        updateMessageTokenTable :: ResourceIO m => SqlPersist m ()
        updateMessageTokenTable = execute sql [mId']
            where sql = " INSERT OR IGNORE INTO \"Token\" (text) \
                        \ SELECT text FROM msg_token \
                        \ WHERE \"messageId\"=?; "

        updateMessageIds :: ResourceIO m => SqlPersist m ()
        updateMessageIds = execute sql [mId']
            where sql = " INSERT OR REPLACE INTO msg_token \
                        \ (\"tokenId\", \"messageId\", text) \
                        \ SELECT t.id, mt.\"messageId\", t.text \
                        \ FROM \"Token\" t \
                        \ JOIN msg_token mt ON mt.text=t.text \
                        \ WHERE mt.\"messageId\"=?; "

        updateMessageIndex :: ResourceIO m => SqlPersist m ()
        updateMessageIndex = execute sql [mId']
            where sql = " INSERT INTO \"Position\" \
                        \ (\"tokenId\", \"messageId\") \
                        \ SELECT \"tokenId\", \"messageId\" \
                        \ FROM msg_token \
                        \ WHERE \"messageId\"=?; "

        cleanUp :: ResourceIO m => SqlPersist m ()
        cleanUp = execute sql [mId']
            where sql = " DELETE FROM msg_token WHERE \"messageId\"=?; "

