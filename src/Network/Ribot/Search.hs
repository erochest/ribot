
-- This module contains functions related to indexing and searching messages.
--
-- The search engine is intentionally basic, and probably won't scale well.
-- However, it should be dead-simple to get going. This just uses an inverted
-- index based on the database tables `token` and `position`, pointing to rows
-- in the table `message`.
--
-- NB: The `position` table implies that this is a word-level index, but
-- actually it's a document-level (i.e., message-level) index.

module Network.Ribot.Search
    ( index
    , tokenize
    , reIndex
    ) where

import qualified Data.List as L
import           Database.HDBC
import           Database.HDBC.Types (IConnection)

-- This takes a database connection, message ID, and message string. It
-- tokenizes and cleans up the string, and indexes the string. It returns the
-- token ized, cleaned up message.
--
-- The index is populated using these steps:
index :: IConnection a => a -> Int -> String -> IO [String]
index cxn msgId msg = do
    createTable cxn
    loadTokens cxn
    updateTokenTable cxn
    updateIds cxn
    updateIndex cxn
    dropTable cxn
    return tokens
    where
        tokens = tokenize msg
        tokenValues = map ((:[]) . toSql) tokens

        -- 1. A temporary table is created to hold the tokens types (unique
        -- tokens) and their ID in `token` for the tokens in the message;
        createTable :: IConnection a => a -> IO ()
        createTable =
            (flip runRaw) "CREATE TEMPORARY TABLE msg_tokens \
                          \ (id INTEGER DEFAULT NULL, \
                          \  text TEXT UNIQUE ON CONFLICT IGNORE);"

        -- 2. The tokens are loaded into the temporary table;
        loadTokens :: IConnection a => a -> IO ()
        loadTokens cxn = do
            stmt <- prepare cxn "INSERT INTO msg_tokens (text) VALUES (?);"
            executeMany stmt tokenValues

        -- 3. `token` is updated with the tokens in the temporary table
        -- (`INSERT OR IGNORE`);
        updateTokenTable :: IConnection a => a -> IO ()
        updateTokenTable =
            (flip runRaw) "INSERT OR IGNORE INTO token (text) \
                          \ SELECT text FROM msg_tokens;"

        -- 4. The IDs in the temporary table are filled in from the `token`
        -- table;
        updateIds :: IConnection a => a -> IO ()
        updateIds =
            (flip runRaw) "INSERT OR REPLACE INTO msg_tokens \
                          \ (id, text) \
                          \ SELECT t.id, t.text \
                          \ FROM token t \
                          \ JOIN msg_tokens mt ON mt.text=t.text;"

        -- 5. The `positions` in the temporary table are inserted from the
        -- temporary table; and
        updateIndex :: IConnection a => a -> IO ()
        updateIndex cxn = do
            stmt <- prepare cxn "INSERT INTO position (token_id, message_id) \
                                \ SELECT id, ? FROM msg_tokens;"
            execute stmt [toSql msgId]
            return ()

        -- 6. Clean up.
        dropTable :: IConnection a => a -> IO ()
        dropTable = (flip runRaw) "DROP TABLE msg_tokens;" 

-- This takes an input string and tokenizes it and removes stop words and
-- punctuation.
tokenize :: String -> [String]
tokenize _ = []

-- This takes a database connection and it re-indexes the entire set of
-- messages it contains. It returns the number of messages indexed and the
-- number of tokens indexed.
reIndex :: IConnection a => a -> IO (Int, Int)
reIndex _ = return (-1, -1)

