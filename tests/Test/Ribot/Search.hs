{-# LANGUAGE FlexibleContexts #-}

module Test.Ribot.Search (searchTests) where

import           Control.Exception (bracket)
import           Data.Convertible (Convertible)
import qualified Data.List as L
import           Database.HDBC
import           Database.HDBC.Sqlite3 (connectSqlite3, Connection)
-- import           Database.HDBC.Types (IConnection(..))
import           Network.Ribot.Db (createDb, initDb)
import           Network.Ribot.Search
import           Test.HUnit (Assertion, assertBool)
import           Test.Framework (Test, testGroup)
import           Test.Framework.Providers.HUnit (testCase)

assertTokenizeMessage :: Assertion
assertTokenizeMessage = do
    assertBool "assertTokenizeMessage plain"
               ((tokenize "" "big important words") == expected)
    assertBool "assertTokenizeMessage stop words"
               ((tokenize "" "this has big important words") == expected)
    assertBool "assertTokenizeMessage punctuation"
               ((tokenize "" "this has big, important words!") == expected)
    assertBool "assertTokenizeMessage normalize"
               ((tokenize "" "This has BIG, important words!") == expected)
    where expected = ["big", "important", "words"]

-- This creates a temporary database with the schema for ribot and runs the
-- test `Assertion` in it.
withTempDb :: (Connection -> Assertion) -> Assertion
withTempDb = bracket (connectSqlite3 ":memory:" >>= initDb >>= createDb)
                     disconnect

-- This picks the scalar result from the returned query list.
getScalar :: Convertible SqlValue a => [[SqlValue]] -> a
getScalar = fromSql . L.head . L.head

-- This gets the `LAST_INSERT_ROWID()`.
lastInsertRowId :: IConnection c => c -> IO Int
lastInsertRowId cxn =
    quickQuery' cxn "SELECT LAST_INSERT_ROWID();" [] >>=
    return . getScalar

-- Insert a user into the database. Its returns the user's ID.
insertUser :: IConnection c => c -> String -> IO Int
insertUser cxn nick =
    run cxn
        "INSERT INTO user (username, logging_on) \
        \ VALUES (?, 1);"
        [toSql nick] >>
    lastInsertRowId cxn

-- Insert a message into the database. It returns the message's ID.
insertMsg :: IConnection c => c -> Int -> String -> IO Int
insertMsg cxn uId m =
    run cxn
        "INSERT INTO message (user_id, text, posted) \
        \ VALUES (?, ?, DATETIME('NOW'));"
        [toSql uId, toSql m] >>
    lastInsertRowId cxn

-- This tests indexing a single message.
assertIndexMessage :: Assertion
assertIndexMessage =
    withTempDb $ \cxn -> do
        userId <- insertUser cxn "zaphod"
        msgId  <- insertMsg cxn userId msg

        tokens <- index cxn "zaphod" msgId msg
        assertBool ("assertIndexMessage indexed tokens: " ++ (show tokens))
                   (tokens == ["aboard", "heart", "gold"])
        dbTokens <- getDbTokens cxn
        assertBool ("assertIndexMessage tokens in db: " ++ (show dbTokens))
                   (dbTokens == ["aboard", "gold", "heart"])
        msgTokens <- getMsgTokens cxn msgId
        assertBool ("assertIndexMessage tokens in msg: " ++ (show msgTokens))
                   (msgTokens == ["aboard", "gold", "heart"])

    where
        msg = "Aboard the Heart of Gold!"

        getDbTokens :: IConnection c => c -> IO [String]
        getDbTokens cxn = do
            results <- quickQuery' cxn
                                   "SELECT text FROM token ORDER BY text;"
                                   []
            return [fromSql text | [text] <- results]

        getMsgTokens :: IConnection c => c -> Int -> IO [String]
        getMsgTokens cxn mId = do
            results <- quickQuery' cxn
                                   "SELECT t.text \
                                   \ FROM token t \
                                   \ JOIN position p ON p.token_id=t.id \
                                   \ WHERE p.message_id=?;"
                                   [toSql mId]
            return [fromSql text | [text] <- results]

-- This tests indexing multiple messages with some overlapping tokens.
assertIndexMessages :: Assertion
assertIndexMessages =
    withTempDb $ \cxn -> do
        assertBool "assertIndexMessages" False

searchTests :: [Test]
searchTests =
    [ testGroup "search" [ testCase "tokenize-message" assertTokenizeMessage
                         , testCase "index-message" assertIndexMessage
                         , testCase "index-messages" assertIndexMessages
                         ]
    ]

