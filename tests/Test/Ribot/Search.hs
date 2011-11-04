
module Test.Ribot.Search (searchTests) where

import           Database.HDBC
import           Network.Ribot.Search
import           Test.HUnit (Assertion, assertBool)
import           Test.Framework (Test, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.Ribot.Db

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

