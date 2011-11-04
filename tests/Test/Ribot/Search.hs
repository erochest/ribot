
module Test.Ribot.Search (searchTests) where

import           Control.Exception (bracket)
import           Database.HDBC
import           Database.HDBC.Sqlite3 (connectSqlite3, Connection)
-- import           Database.HDBC.Types (IConnection(..))
import           Network.Ribot.Db (createDb, initDb)
import           Network.Ribot.Search
import           Test.HUnit (Assertion, assertBool)
import           Test.Framework (Test, testGroup)
import           Test.Framework.Providers.HUnit (testCase)

withTempDb :: (Connection -> Assertion) -> Assertion
withTempDb = bracket (connectSqlite3 ":memory:" >>= initDb >>= createDb)
                     disconnect

assertIndexMessage :: Assertion
assertIndexMessage =
    withTempDb $ \cxn -> do
        insertUser cxn userId "zaphod"
        insertMsg cxn userId msgId msg
        tokens <- index cxn "zaphod" msgId msg
        assertBool "assertIndexMessage indexed tokens"
                   (tokens == ["all", "aboard", "heart", "gold"])
        dbTokens <- getDbTokens cxn
        assertBool "assertIndexMessage tokens in db"
                   (dbTokens == ["aboard", "all", "gold", "heart"])
        msgTokens <- getMsgTokens cxn msgId
        assertBool "assertIndexMessage tokens in msg"
                   (msgTokens == ["aboard", "all", "gold", "heart"])

    where
        userId = 3
        msgId = 4
        msg = "All aboard the Heart of Gold!"

        insertUser :: IConnection c => c -> Int -> String -> IO ()
        insertUser cxn uId nick =
            run cxn
                "INSERT INTO user (id, username, logging_on) \
                \ VALUES (?, ?, 0);"
                [toSql uId, toSql nick] >>
            return ()

        insertMsg :: IConnection c => c -> Int -> Int -> String -> IO ()
        insertMsg cxn uId mId m =
            run cxn
                "INSERT INTO message (id, user_id, text, posted) \
                \ VALUES (?, ?, ?, DATETIME('NOW'));"
                [toSql mId, toSql uId, toSql m] >>
            return ()

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

searchTests :: [Test]
searchTests =
    [ testGroup "search" [ testCase "index-message" assertIndexMessage
                         , testCase "tokenize-message" assertTokenizeMessage
                         ]
    ]

