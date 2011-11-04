
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
    , reindex
    , search
    , parseSearch
    , buildQuery
    , SearchResult
    ) where

import           Control.Monad (mapM_)
import qualified Data.Char as C
import qualified Data.List as L
import           Database.HDBC
import           Database.HDBC.Types (IConnection)
import           Text.ParserCombinators.Parsec
import qualified Text.Ribot.Tokenizer as T

-- This takes a database connection, message ID, and message string. It
-- tokenizes and cleans up the string, and indexes the string. It returns the
-- token ized, cleaned up message.
--
-- The index is populated using these steps:
index :: IConnection a => a -> String -> Int -> String -> IO [String]
index cxn nick msgId msg =
    withTransaction cxn $ \cxn -> do
        loadTokens cxn
        updateTokenTable cxn
        updateIds cxn
        updateIndex cxn
        cleanUp cxn
        return tokens
    where
        tokens = tokenize nick msg
        tokenValues = [[msgIdSql, toSql tkn] | tkn <- tokens]
        msgIdSql = toSql msgId

        -- 1. The tokens are loaded into the temporary table;
        loadTokens :: IConnection a => a -> IO ()
        loadTokens cxn = do
            stmt <- prepare cxn "INSERT OR IGNORE INTO msg_token \
                                \ (message_id, text) VALUES (?, ?);"
            executeMany stmt tokenValues
            return ()

        -- 2. `token` is updated with the tokens in the temporary table
        -- (`INSERT OR IGNORE`);
        updateTokenTable :: IConnection a => a -> IO ()
        updateTokenTable cxn = do
            stmt <- prepare cxn "INSERT OR IGNORE INTO token (text) \
                                \ SELECT text FROM msg_token \
                                \ WHERE message_id=?;"
            execute stmt [msgIdSql]
            return ()

        -- 3. The IDs in the temporary table are filled in from the `token`
        -- table;
        updateIds :: IConnection a => a -> IO ()
        updateIds cxn = do
            stmt <- prepare cxn "INSERT OR REPLACE INTO msg_token \
                                \ (token_id, message_id, text) \
                                \ SELECT t.id, mt.message_id, t.text \
                                \ FROM token t \
                                \ JOIN msg_token mt ON mt.text=t.text \
                                \ WHERE mt.message_id=?;"
            execute stmt [msgIdSql]
            return ()

        -- 4. The tokens in the temporary table are inserted into the
        -- `position` table; and
        updateIndex :: IConnection a => a -> IO ()
        updateIndex cxn = do
            stmt <- prepare cxn "INSERT INTO position (token_id, message_id) \
                                \ SELECT token_id, message_id FROM msg_token \
                                \ WHERE message_id=?;"
            execute stmt [msgIdSql]
            return ()

        -- 5. Remove the tokens from this message from the temporary table.
        cleanUp :: IConnection a => a -> IO ()
        cleanUp cxn = do
            stmt <- prepare cxn "DELETE FROM msg_token WHERE message_id=?;"
            execute stmt [msgIdSql]
            return ()

-- This takes an input string and tokenizes it and removes stop words and
-- punctuation. The parameters are the user's nick and the message.
tokenize :: String -> String -> [String]
tokenize nick input = 
    case (T.tokenize nick input) of
        Left _    -> []
        Right all -> L.filter keep all
    where
        keep :: String -> Bool
        keep word = T.isWord word && not (T.inStopList word)
    

-- This takes a database connection and it re-indexes the entire set of
-- messages it contains. It returns the number of messages indexed and the
-- number of tokens indexed.
reindex :: IConnection a => a -> IO (Int, Int)
reindex cxn = do
    withTransaction cxn $ \cxn -> do
        clearExistingData cxn

        msgs <- getMessages cxn
        let tokens = tokenizeMessages msgs

        populateMsgTokenTable cxn tokens
        updateTokenTable cxn
        updateIds cxn
        updateIndex cxn

        cleanUp cxn
        return (length msgs, length tokens)

    where
        -- 1. First, we have to clear out the existing data from the `token`
        -- and `position` tables;
        clearExistingData :: IConnection a => a -> IO ()
        clearExistingData cxn =
            mapM_ (runRaw cxn) [ "DELETE FROM token;"
                               , "DELETE FROM position;"
                               , "DELETE FROM msg_token;"
                               ]

        -- 2. Get all messages (Message ID, User Nick, Message Text);
        getMessages :: IConnection a => a -> IO [(Int, String, String)]
        getMessages cxn = do
            results <- quickQuery' cxn
                                   " SELECT m.id, u.username, m.text \
                                   \ FROM message m \
                                   \ JOIN user u ON u.id=m.user_id;"
                                   []
            return [(fromSql mId, fromSql userName, fromSql msgText) |
                    [mId, userName, msgText] <- results]

        -- 3. Tokenize them (output of `getMessages` -> (mId, tokenText));
        tokenizeMessages :: [(Int, String, String)] -> [(Int, String)]
        tokenizeMessages = L.concatMap tokenizeMessage

        tokenizeMessage :: (Int, String, String) -> [(Int, String)]
        tokenizeMessage (msgId, nick, msgText) =
            map ((,) msgId) $ tokenize nick msgText

        -- 4. Push the tokens into `msg_token`;
        populateMsgTokenTable :: IConnection c => c -> [(Int, String)] -> IO ()
        populateMsgTokenTable cxn tokens = do
            stmt <- prepare cxn " INSERT OR IGNORE INTO msg_token \
                                \ (message_id, text) VALUES (?, ?);"
            executeMany stmt [[toSql msgId, toSql token] |
                              (msgId, token) <- tokens]
            return ()

        -- 5. Fill in the `token` table;
        updateTokenTable :: IConnection c => c -> IO ()
        updateTokenTable cxn =
            runRaw cxn " INSERT OR IGNORE INTO token (text) \
                       \ SELECT text FROM msg_token;"

        -- 6. Pull the token IDs back into `msg_token` (unfortunately, I can't
        -- figure out a way to do this in SQL without `UPDATE ... SELECT`);
        updateIds :: IConnection a => a -> IO ()
        updateIds cxn = do
            tokens <- quickQuery' cxn "SELECT id, text FROM token;" []
            update <- prepare cxn " UPDATE msg_token \
                                  \ SET token_id=? WHERE text=?;"
            executeMany update tokens

        -- 7. Push the token/message relationships into `position`; and
        updateIndex :: IConnection a => a -> IO ()
        updateIndex cxn =
            runRaw cxn " INSERT INTO position (token_id, message_id) \
                       \ SELECT token_id, message_id FROM msg_token; "

        -- 8. Remove the working data from the `msg_token` table.
        cleanUp :: IConnection a => a -> IO ()
        cleanUp cxn =
            runRaw cxn "DELETE FROM msg_token;"


-- A `SearchResult` is one hit from a search query. It contains
-- 1. the message ID,
-- 2. the nick of the person who sent the message,
-- 3. the date the message was sent,
-- 4. the token that was hit, and
-- 5. the message text.
type SearchResult = (Int, String, String, String)

-- This parses a search query, turns it into SQL, passes it to the database,
-- parses the results into a list of `SearchResult`s and returns them.
search :: IConnection c => c -> String -> IO [SearchResult]
search cxn query = do
    fmap (map toSearchResult) $ quickQuery' cxn sql params
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

-- Parse search parses the search query according to these rules:
-- * for the most part, tokenization follows `tokenize` above;
-- * English stop words and punctuation are stripped out; and
-- * wildcards (stars) are turned into SQL wildcards (*%*).
parseSearch :: String -> [String]
parseSearch input =
    case (tokenizeQuery "<query>" input) of
        Left _    -> []
        Right all -> map normalize $ L.filter keep all
    where
        -- This handles interacting with the parser.
        tokenizeQuery :: String -> String -> Either ParseError [String]
        tokenizeQuery = parse tokenList

        -- This is a predicate defining the tokens that need to be kept.
        keep :: String -> Bool
        keep = not . T.inStopList

        -- This fixes the wildcard characters in a string and lower-cases the
        -- characters.
        normalize :: String -> String
        normalize []       = []
        normalize ('*':xs) = '%' : normalize xs
        normalize (x:xs)   = (C.toLower x) : (normalize xs)

        -- This defines a list of tokens.
        tokenList :: GenParser Char st [String]
        tokenList = word `sepBy` many (satisfy isJunk)

        -- A single token. This is where it differs from
        -- `Text.Ribot.Tokenizer`.
        word :: GenParser Char st String
        word = do
            first <- alphaNum <|> char '*'
            rest <- many (alphaNum <|> char '*' <|> oneOf ".")
            return (first:rest)

        -- This is a predicate defining a junk character. It's basically a
        -- predicate inverse of `word`.
        isJunk :: Char -> Bool
        isJunk '*'                = False
        isJunk c | C.isAlphaNum c = False
        isJunk _                  = True

-- This takes a processed search query (the output of `parseSearch`) and it
-- returns a SQL statement that takes those terms and returns the parameters
-- contained in `SearchResult`. It also returns the terms as `SqlValue`s.
buildQuery :: [String] -> (String, [SqlValue])
buildQuery queryTerms =
    (L.concat . L.reverse . (suffix:) $ wheres ++ sqlList, L.reverse params)
    where
        -- The initial state for the fold
        init = ([prefix], [], [])

        -- The results of the folding the query terms to accumulate the
        -- intermediate output.
        (sqlList, wheres, params) = L.foldl' foldTerm init . zip [0..] $ queryTerms

        -- This is the prefix for all queries.
        prefix = "SELECT DISTINCT m.id, u.username, m.posted, m.text\
                 \ FROM message m\
                 \ JOIN user u ON u.id=m.user_id"
        -- This is the suffix for all queries.
        suffix = " ORDER BY m.posted DESC LIMIT 25;"

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
                  op    = case ('%' `elem` term) of
                            True  -> " LIKE "
                            False -> "="
                  n' = show n

        foldTerm :: ([String], [String], [SqlValue])
                 -> (Int, String)
                 -> ([String], [String], [SqlValue])
        foldTerm (sql, wheres, params) (n, term) =
            ((join n : sql), (where_ n term : wheres), (toSql term) : params)

