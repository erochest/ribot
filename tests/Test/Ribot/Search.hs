
module Test.Ribot.Search (searchTests) where

import           Test.HUnit (Assertion, assertBool)
import           Test.Framework (Test, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
-- import           Text.Ribot.Search

assertIndexMessage :: Assertion
assertIndexMessage = assertBool "assertIndexMessage" False

assertTokenizeMessage :: Assertion
assertTokenizeMessage = assertBool "assertTokenizeMessage" False

searchTests :: [Test]
searchTests =
    [ testGroup "search" [ testCase "index-message" assertIndexMessage
                         , testCase "tokenize-message" assertTokenizeMessage
                         ]
    ]

