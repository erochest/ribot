
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

assertSearch :: String -> String -> [String] -> Assertion
assertSearch title input expected =
    assertBool (title ++ ": " ++ (show tokens))
               (expected == tokens)
    where tokens = parseSearch input

assertParseSearchSingle :: Assertion
assertParseSearchSingle = do
    assertSearch' "message" ["message"]
    assertSearch' "42" ["42"]
    assertSearch' "the" []
    assertSearch' "." []
    assertSearch' "messages" ["messages"]
    where assertSearch' = assertSearch "assertParseSearchSingle"

assertParseSearchMulti :: Assertion
assertParseSearchMulti = do
    assertSearch' "some message" ["message"]
    assertSearch' "important message" ["important", "message"]
    assertSearch' "42 + 3" ["42", "3"]
    assertSearch' "the messages" ["messages"]
    assertSearch' "3.14159 2.71828183 13 42" ["3.14159", "2.71828183", "13", "42"]
    where assertSearch' = assertSearch "assertParseSearchMulti"

assertParseSearchWild :: Assertion
assertParseSearchWild = do
    assertSearch' "message*" ["message%"]
    assertSearch' "*Message" ["%message"]
    assertSearch' "mes*age" ["mes%age"]
    assertSearch' ".*" []
    assertSearch' "pi 3.14*" ["pi", "3.14%"]
    assertSearch' "pi* 3.14*" ["pi%", "3.14%"]
    where assertSearch' = assertSearch "assertParseSearchWild"

-- NOTE: These tests are a little more low-level than I'd like. There's
-- multiple ways to express these in SQL, and I'd prefer to leave myself the
-- flexibility to account for them.

assertQuery :: String -> String -> String -> [SqlValue] -> Assertion
assertQuery title input expected expectedParams =
    assertBool (title ++ ": " ++ sql ++ " <- " ++ (show params))
               (expected == sql && expectedParams == params)
    where (sql, params) = buildQuery $ parseSearch input

assertQuerySingle :: Assertion
assertQuerySingle =
    assertQuery' query sql params
    where assertQuery' = assertQuery "assertQuerySingle"
          query = "message"
          sql = "SELECT m.id, u.username, m.posted, t.text, m.text\
                \ FROM message m\
                \ JOIN user u ON u.id=m.user_id\
                \ JOIN position p0 ON p0.message_id=m.id\
                \ JOIN token t0 ON t0.id=p0.token_id\
                \ WHERE t0.text=?\
                \ ORDER BY m.posted DESC\
                \ LIMIT 25;"
          params = [toSql "message"]

assertQueryMulti :: Assertion
assertQueryMulti = 
    assertQuery' query sql params
    where assertQuery' = assertQuery "assertQueryMulti"
          query = "important message"
          sql = "SELECT m.id, u.username, m.posted, t.text, m.text\
                \ FROM message m\
                \ JOIN user u ON u.id=m.user_id\
                \ JOIN position p0 ON p0.message_id=m.id\
                \ JOIN token t0 ON t0.id=p0.token_id\
                \ JOIN position p1 ON p1.message_id=m.id\
                \ JOIN token t1 ON t1.id=p1.token_id\
                \ WHERE t0.text=?\
                \ AND t1.text=?\
                \ ORDER BY m.posted DESC\
                \ LIMIT 25;"
          params = [toSql "important", toSql "message"]

assertQueryWildSingle :: Assertion
assertQueryWildSingle = 
    assertQuery' query sql params
    where assertQuery' = assertQuery "assertQueryWildSingle"
          query = "messag*"
          sql = "SELECT m.id, u.username, m.posted, t.text, m.text\
                \ FROM message m\
                \ JOIN user u ON u.id=m.user_id\
                \ JOIN position p0 ON p0.message_id=m.id\
                \ JOIN token t0 ON t0.id=p0.token_id\
                \ WHERE t0.text LIKE ?\
                \ ORDER BY m.posted DESC\
                \ LIMIT 25;"
          params = [toSql "messag%"]

assertQueryWildMulti :: Assertion
assertQueryWildMulti = 
    assertQuery' query sql params
    where assertQuery' = assertQuery "assertQueryWildMulti"
          query = "important messag*"
          sql = "SELECT m.id, u.username, m.posted, t.text, m.text\
                \ FROM message m\
                \ JOIN user u ON u.id=m.user_id\
                \ JOIN position p0 ON p0.message_id=m.id\
                \ JOIN token t0 ON t0.id=p0.token_id\
                \ JOIN position p1 ON p1.message_id=m.id\
                \ JOIN token t1 ON t1.id=p1.token_id\
                \ WHERE t0.text=?\
                \ AND t1.text LIKE ?\
                \ ORDER BY m.posted DESC\
                \ LIMIT 25;"
          params = [toSql "important", toSql "messag%"]

assertQueryEmpty :: Assertion
assertQueryEmpty = 
    assertQuery' query sql params
    where assertQuery' = assertQuery "assertQueryEmpty"
          query = ""
          sql = "SELECT m.id, u.username, m.posted, t.text, m.text\
                \ FROM message m\
                \ JOIN user u ON u.id=m.user_id\
                \ ORDER BY m.posted DESC\
                \ LIMIT 25;"
          params = []

testMessages :: [String]
testMessages = [ "This is a small message to test multiple indexing message."
               , "This is another message for testing indexing multiple messages."
               ]

assertSearchResults :: String -> String -> [Int] -> Assertion
assertSearchResults title query expected =
    withTempDb $ \cxn -> do
        aId <- insertUser cxn "a"
        mapM_ (insertMsg cxn aId) testMessages
        _ <- reindex cxn

        results <- search cxn query
        let mIds = map resultMessageId results

        assertBool (title ++ ": " ++ (show mIds))
                   (expected == mIds)

    where resultMessageId :: SearchResult -> Int
          resultMessageId (mId, _, _, _, _) = mId

assertSearchSingle :: Assertion
assertSearchSingle = do
    assertResults' "message" [1, 2]
    assertResults' "messages" [2]
    assertResults' "messaging" []
    where assertResults' = assertSearchResults "assertSearchSingle"

assertSearchMulti :: Assertion
assertSearchMulti = do
    assertResults' "multiple message" [1, 2]
    assertResults' "small multiple message" [1]
    assertResults' "another small multiple message" []
    where assertResults' = assertSearchResults "assertSearchMulti"

assertSearchWild :: Assertion
assertSearchWild = do
    assertResults' "messag*" [1, 2]
    assertResults' "t* messag*" [1, 2]
    assertResults' "*ing t* messag*" [1, 2]
    where assertResults' = assertSearchResults "assertSearchWild"

searchTests :: [Test]
searchTests =
    [ testGroup "indexing" [ testCase "tokenize-message" assertTokenizeMessage
                           , testCase "index-message" assertIndexMessage
                           , testCase "index-messages" assertIndexMessages
                           , testCase "reindex" assertReindex
                           ]
    , testGroup "search"   [ testCase "parse-single" assertParseSearchSingle
                           , testCase "parse-multiple" assertParseSearchMulti
                           , testCase "parse-wildcard" assertParseSearchWild
                           , testCase "query-single" assertQuerySingle
                           , testCase "query-multiple" assertQueryMulti
                           , testCase "query-wildcard" assertQueryWildSingle
                           , testCase "query-wildcards" assertQueryWildMulti
                           , testCase "query-empty" assertQueryEmpty
                           , testCase "search-single" assertSearchSingle
                           , testCase "search-multiple" assertSearchMulti
                           , testCase "search-wild" assertSearchWild
                           ]
    ]

