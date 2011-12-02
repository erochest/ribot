
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
    , reindex
    , search
    , buildQuery
    , showSearchResult
    , SearchResult
    ) where

import qualified Data.List as L
import           Database.HDBC

-- [Network.Ribot.Types](Types.html)
-- [Text.Ribot.Tokenizer](../../Text/Ribot/Tokenizer.html) <br />
-- [Text.Ribot.Search](../../Text/Ribot/Search.html)
import           Text.Ribot.Search

-- This takes a database connection, message ID, and message string. It
-- tokenizes and cleans up the string, and indexes the string. It returns the
-- token ized, cleaned up message.
--
-- The index is populated using these steps:
index :: IConnection a => a -> String -> Int -> String -> IO [String]
index cxn nick msgId msg =
    withTransaction cxn $ \db -> do
        loadMessageTokens db
        updateMessageTokenTable db
        updateMessageIds db
        updateMessageIndex db
        cleanUp db
        return tokens
    where
        tokens = tokenize nick msg
        tokenValues = [[msgIdSql, toSql tkn] | tkn <- tokens]
        msgIdSql = toSql msgId

        -- * The tokens are loaded into the temporary table;
        loadMessageTokens :: IConnection a => a -> IO ()
        loadMessageTokens db = do
            stmt <- prepare db "INSERT OR IGNORE INTO msg_token \
                               \ (message_id, text) VALUES (?, ?);"
            executeMany stmt tokenValues
            return ()

        -- * `token` is updated with the tokens in the temporary table
        --   (`INSERT OR IGNORE`);
        updateMessageTokenTable :: IConnection a => a -> IO ()
        updateMessageTokenTable db = do
            stmt <- prepare db "INSERT OR IGNORE INTO token (text) \
                               \ SELECT text FROM msg_token \
                               \ WHERE message_id=?;"
            execute stmt [msgIdSql]
            return ()

        -- * The IDs in the temporary table are filled in from the `token`
        --   table;
        updateMessageIds :: IConnection a => a -> IO ()
        updateMessageIds db = do
            stmt <- prepare db "INSERT OR REPLACE INTO msg_token \
                               \ (token_id, message_id, text) \
                               \ SELECT t.id, mt.message_id, t.text \
                               \ FROM token t \
                               \ JOIN msg_token mt ON mt.text=t.text \
                               \ WHERE mt.message_id=?;"
            execute stmt [msgIdSql]
            return ()

        -- * The tokens in the temporary table are inserted into the
        --   `position` table; and
        updateMessageIndex :: IConnection a => a -> IO ()
        updateMessageIndex db = do
            stmt <- prepare db "INSERT INTO position (token_id, message_id) \
                               \ SELECT token_id, message_id FROM msg_token \
                               \ WHERE message_id=?;"
            execute stmt [msgIdSql]
            return ()

        -- * Remove the tokens from this message from the temporary table.
        cleanUp :: IConnection a => a -> IO ()
        cleanUp db = do
            stmt <- prepare db "DELETE FROM msg_token WHERE message_id=?;"
            execute stmt [msgIdSql]
            return ()

-- This takes a database connection and it re-indexes the entire set of
-- messages it contains. It returns the number of messages indexed and the
-- number of tokens indexed.
reindex :: IConnection a => a -> IO (Int, Int)
reindex cxn =
    withTransaction cxn $ \db -> do
        clearExistingData db

        msgs <- getMessages db
        let tokens = tokenizeMessages msgs

        populateMsgTokenTable db tokens
        updateTokenTable db
        updateIds db
        updateIndex db

        cleanUp db
        return (length msgs, length tokens)

    where
        -- * First, we have to clear out the existing data from the `token`
        --   and `position` tables;
        clearExistingData :: IConnection a => a -> IO ()
        clearExistingData db =
            mapM_ (runRaw db) [ "DELETE FROM token;"
                              , "DELETE FROM position;"
                              , "DELETE FROM msg_token;"
                              ]

        -- * Get all messages (Message ID, User Nick, Message Text);
        getMessages :: IConnection a => a -> IO [(Int, String, String)]
        getMessages db = do
            results <- quickQuery' db
                                   " SELECT m.id, u.username, m.text \
                                   \ FROM message m \
                                   \ JOIN user u ON u.id=m.user_id;"
                                   []
            return [(fromSql mId, fromSql userName, fromSql msgText) |
                    [mId, userName, msgText] <- results]

        -- * Tokenize them (output of `getMessages` -> (mId, tokenText));
        tokenizeMessages :: [(Int, String, String)] -> [(Int, String)]
        tokenizeMessages = L.concatMap tokenizeMessage

        tokenizeMessage :: (Int, String, String) -> [(Int, String)]
        tokenizeMessage (msgId, nick, msgText) =
            map ((,) msgId) $ tokenize nick msgText

        -- * Push the tokens into `msg_token`;
        populateMsgTokenTable :: IConnection c => c -> [(Int, String)] -> IO ()
        populateMsgTokenTable db tokens = do
            stmt <- prepare db " INSERT OR IGNORE INTO msg_token \
                               \ (message_id, text) VALUES (?, ?);"
            executeMany stmt [[toSql msgId, toSql token] |
                              (msgId, token) <- tokens]
            return ()

        -- * Fill in the `token` table;
        updateTokenTable :: IConnection c => c -> IO ()
        updateTokenTable db =
            runRaw db " INSERT OR IGNORE INTO token (text) \
                      \ SELECT text FROM msg_token;"

        -- * Pull the token IDs back into `msg_token` (unfortunately, I can't
        -- figure out a way to do this in SQL without `UPDATE ... SELECT`);
        updateIds :: IConnection a => a -> IO ()
        updateIds db = do
            tokens <- quickQuery' db "SELECT id, text FROM token;" []
            update <- prepare db " UPDATE msg_token \
                                 \ SET token_id=? WHERE text=?;"
            executeMany update tokens

        -- * Push the token/message relationships into `position`; and
        updateIndex :: IConnection a => a -> IO ()
        updateIndex db =
            runRaw db " INSERT INTO position (token_id, message_id) \
                      \ SELECT token_id, message_id FROM msg_token; "

        -- * Remove the working data from the `msg_token` table.
        cleanUp :: IConnection a => a -> IO ()
        cleanUp db =
            runRaw db "DELETE FROM msg_token;"

-- A `SearchResult` is one hit from a search query. It contains
--
-- 1. the message ID,
-- 2. the nick of the person who sent the message,
-- 3. the date the message was sent, and
-- 4. the message text.
type SearchResult = (Int, String, String, String)

-- This parses a search query, turns it into SQL, passes it to the database,
-- parses the results into a list of `SearchResult`s and returns them.
search :: IConnection c => c -> String -> IO [SearchResult]
search cxn query = fmap (map toSearchResult) $ quickQuery' cxn sql params
    where
        -- These are the SQL query to run and its parameters.
        (sql, params) = buildQuery $ parseSearch query

        -- This converts a result row to a `SearchResult`.
        toSearchResult :: [SqlValue] -> SearchResult
        toSearchResult [mId, userName, posted, mText] =
            ( fromSql mId
            , fromSql userName
            , fromSql posted
            , fromSql mText
            )
        toSearchResult _ = (-1, "", "", "")

-- This takes a processed search query (the output of `parseSearch`) and it
-- returns a SQL statement that takes those terms and returns the parameters
-- contained in `SearchResult`. It also returns the terms as `SqlValue`s.
buildQuery :: [String] -> (String, [SqlValue])
buildQuery queryTerms =
    (L.concat . L.reverse . (suffix:) $ wheres ++ sqlList, L.reverse params)
    where
        -- The initial state for the fold
        foldInit = ([prefix], [], [])

        -- The results of the folding the query terms to accumulate the
        -- intermediate output.
        (sqlList, wheres, params) =
            L.foldl' foldTerm foldInit . zip [0..] $ queryTerms

        -- This is the prefix for all queries.
        prefix = "SELECT DISTINCT m.id, u.username, m.posted, m.text\
                 \ FROM message m\
                 \ JOIN user u ON u.id=m.user_id"
        -- This is the suffix for all queries.
        suffix = " ORDER BY m.posted DESC;"

        -- This creates a join clause for a given item in the term list.
        join :: Int -> String
        join n = " JOIN position p" ++ n' ++ " ON p" ++ n' ++ ".message_id=m.id\
                 \ JOIN token t" ++ n' ++ " ON t" ++ n' ++ ".id=p" ++ n' ++ ".token_id"
            where n' = show n

        -- This creates a where clause for a given item in the term list.
        where_ :: Int -> String -> String
        where_ n term = intro ++ " t" ++ n' ++ ".text" ++ op ++ "?"
            where intro = case n of
                            0 -> " WHERE"
                            _ -> " AND"
                  op    = if '%' `elem` term
                            then " LIKE "
                            else "="
                  n' = show n

        foldTerm :: ([String], [String], [SqlValue])
                 -> (Int, String)
                 -> ([String], [String], [SqlValue])
        foldTerm (sql, wheres', params') (n, term) =
            (join n : sql, where_ n term : wheres', toSql term : params')

-- This constructs a string for the output of the search results.
showSearchResult :: SearchResult -> String
showSearchResult (_, nick, date, message) =
    "[" ++ date ++ "] " ++ nick ++ ": " ++ message

