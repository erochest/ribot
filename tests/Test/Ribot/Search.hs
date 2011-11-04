
module Test.Ribot.Search (searchTests) where

import           Control.Exception (bracket)
import           Database.HDBC
import           Database.HDBC.Sqlite3 (connectSqlite3, Connection)
-- import           Database.HDBC.Types (IConnection(..))
import           Network.Ribot.Db (createDb, initDb)
-- import           Network.Ribot.Search
import           Test.HUnit (Assertion, assertBool)
import           Test.Framework (Test, testGroup)
import           Test.Framework.Providers.HUnit (testCase)

withTempDb :: (Connection -> Assertion) -> Assertion
withTempDb = bracket (connectSqlite3 ":memory:" >>= initDb >>= createDb)
                     disconnect

assertIndexMessage :: Assertion
assertIndexMessage =
    withTempDb $ \_ -> assertBool "assertIndexMessage" False

assertTokenizeMessage :: Assertion
assertTokenizeMessage = assertBool "assertTokenizeMessage" False

searchTests :: [Test]
searchTests =
    [ testGroup "search" [ testCase "index-message" assertIndexMessage
                         , testCase "tokenize-message" assertTokenizeMessage
                         ]
    ]

