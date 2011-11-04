
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
        aId <- insertUser cxn "a"
        bId <- insertUser cxn "b"
        ma1Id <- insertMsg cxn aId ma1
        mb1Id <- insertMsg cxn bId mb1

        let expected = [ ("another",  "b", mb1Id)
                       , ("indexing", "a", ma1Id)
                       , ("indexing", "b", mb1Id)
                       , ("message",  "a", ma1Id)
                       , ("message",  "b", mb1Id)
                       , ("messages", "b", mb1Id)
                       , ("multiple", "a", ma1Id)
                       , ("multiple", "b", mb1Id)
                       , ("small",    "a", ma1Id)
                       , ("test",     "a", ma1Id)
                       , ("testing",  "b", mb1Id)
                       ]

        _ <- index cxn "a" ma1Id ma1
        _ <- index cxn "b" mb1Id mb1

        tokens <- getIndex cxn

        assertBool "assertIndexMessages" (expected == tokens)

    where
        ma1 = "This is a small message to test multiple indexing message."
        mb1 = "This is another message for testing indexing multiple messages."

        getIndex :: IConnection c => c -> IO [(String, String, Int)]
        getIndex cxn = do
            results <- quickQuery' cxn
                                   " SELECT t.text, u.username, m.id \
                                   \ FROM position p \
                                   \ JOIN message m ON p.message_id=m.id \
                                   \ JOIN user u ON m.user_id=u.id \
                                   \ JOIN token t ON p.token_id=t.id \
                                   \ ORDER BY t.text, u.username, m.id;"
                                   []
            return [(fromSql text, fromSql user, fromSql mid) |
                    [text, user, mid] <- results]

-- This tests re-indexing multiple messages with some overlapping tokens.
assertReindex :: Assertion
assertReindex =
    withTempDb $ \cxn -> do
        aId <- insertUser cxn "a"
        bId <- insertUser cxn "b"
        ma1Id <- insertMsg cxn aId ma1
        mb1Id <- insertMsg cxn bId mb1

        let expected = [ ("another",  "b", mb1Id)
                       , ("indexing", "a", ma1Id)
                       , ("indexing", "b", mb1Id)
                       , ("message",  "a", ma1Id)
                       , ("message",  "b", mb1Id)
                       , ("messages", "b", mb1Id)
                       , ("multiple", "a", ma1Id)
                       , ("multiple", "b", mb1Id)
                       , ("small",    "a", ma1Id)
                       , ("test",     "a", ma1Id)
                       , ("testing",  "b", mb1Id)
                       ]

        (msgCount, tknCount) <- reindex cxn

        tokens <- getIndex cxn

        assertBool ("assertReindex msgCount: " ++ (show msgCount))
                   (2 == msgCount)
        assertBool ("assertReindex tknCount: " ++ (show tknCount))
                   (12 == tknCount)
        assertBool "assertReindex index" (expected == tokens)

    where
        ma1 = "This is a small message to test multiple indexing message."
        mb1 = "This is another message for testing indexing multiple messages."

        getIndex :: IConnection c => c -> IO [(String, String, Int)]
        getIndex cxn = do
            results <- quickQuery' cxn
                                   " SELECT t.text, u.username, m.id \
                                   \ FROM position p \
                                   \ JOIN message m ON p.message_id=m.id \
                                   \ JOIN user u ON m.user_id=u.id \
                                   \ JOIN token t ON p.token_id=t.id \
                                   \ ORDER BY t.text, u.username, m.id;"
                                   []
            return [(fromSql text, fromSql user, fromSql mid) |
                    [text, user, mid] <- results]


searchTests :: [Test]
searchTests =
    [ testGroup "indexing" [ testCase "tokenize-message" assertTokenizeMessage
                           , testCase "index-message" assertIndexMessage
                           , testCase "index-messages" assertIndexMessages
                           , testCase "reindex" assertReindex
                           ]
    ]

