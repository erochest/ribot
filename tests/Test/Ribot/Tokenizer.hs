{-# LANGUAGE OverloadedStrings #-}

module Test.Ribot.Tokenizer (tokenizerTests) where

import           Data.Monoid (mempty, Monoid)
import qualified Data.Text as T
import           Test.HUnit (Assertion, assertBool)
import           Test.Framework (Test, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Text.Bakers12.Tokenizer.Types (Token(..))
import           Text.Ribot.Tokenizer


-- Some types to make composing the tests from data easier.
type TestPair      = (T.Text, [T.Text])
type TestPairSuite = [TestPair]


-- This returns the empty monoid value for Left Either values.
onlyRight :: Monoid b => Either a b -> b
onlyRight (Left _)    = mempty
onlyRight (Right val) = val


-- This tests one TestPair.
assertToken :: String -> T.Text -> [T.Text] -> Assertion
assertToken descr input expected =
    -- putStrLn . ("!!! " ++) . show $ tokenize "assert" input
    assertBool (descr ++ ": " ++ (show input) ++ " => " ++ (show tokens))
               (expected == tokens)
    where tokens = map tokenText . onlyRight . tokenize "assert" $ input

-- This tests a TestPairSuite list.
assertTokenTests :: String -> TestPairSuite -> Assertion
assertTokenTests descr suite = mapM_ (uncurry (assertToken descr)) suite


-- Now, for actual tests.

assertTokenSimple :: Assertion
assertTokenSimple = assertTokenTests "assertTokenSimple" tests
    where tests = [ ("b", ["b"])
                  , ("x", ["x"])
                  , ("X", ["x"])
                  , ("1", ["1"])
                  , ("8", ["8"])
                  , ("'", [])
                  , ("%", [])
                  , ("\n", [])
                  , (" ", [])
                  , ("abcdef", ["abcdef"])
                  , ("4321", ["4321"])
                  ]

assertTokenMultiple :: Assertion
assertTokenMultiple = assertTokenTests "assertTokenMultiple" tests
    where tests = [ ("b c d", ["b", "c", "d"])
                  , ("E f G", ["e", "f", "g"])
                  , ("H\tJ\nK", ["h", "j", "k"])
                  , ("abc DeF gHi", ["abc", "def", "ghi"])
                  , ("1 2 3", ["1", "2", "3"])
                  , ("abc 123 4H5", ["abc", "123", "4", "h", "5"])
                  , ("abc%1234!!fdjs", ["abc", "1234", "fdjs"])
                  , ("can't won't haven't Bobby's", ["won", "haven", "bobby"])
                  , ("high-five low-five ice-cream", ["high", "five", "low", "five", "ice", "cream"])
                  , ("a1b2 ab12cd34", ["1", "b", "2", "ab", "12", "cd", "34"])
                  ]

assertTokenStopListed :: Assertion
assertTokenStopListed = assertTokenTests "assertTokenStopListed" tests
    where tests = map (flip (,) []) [ "A"
                                    , "I"
                                    , "the"
                                    , "it"
                                    , "he"
                                    , "can"
                                    , "will"
                                    , "against"
                                    ]


tokenizerTests :: [Test]
tokenizerTests =
    [ testGroup "tokenizer" [ testCase "simple" assertTokenSimple
                            , testCase "multiple" assertTokenMultiple
                            , testCase "stop-list" assertTokenStopListed
                            ]
    ]

