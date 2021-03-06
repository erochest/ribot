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
    assertBool (descr ++ ": " ++ show input ++ " => " ++ show tokens)
               (expected == tokens)
    where tokens = map tokenText . onlyRight . tokenize "assert" $ input

-- This tests a TestPairSuite list.
assertTokenTests :: String -> TestPairSuite -> Assertion
assertTokenTests descr = mapM_ (uncurry (assertToken descr))

-- Test tests one TestPair against the query tokenizer.
assertQuery :: String -> T.Text -> [T.Text] -> Assertion
assertQuery descr input expected =
    assertBool (descr ++ ": " ++ show input ++ " => " ++ show tokens)
               (expected == tokens)
    where tokens = map tokenText . onlyRight . tokenizeQuery "assert" $ input

-- This tests a TestPairSuite against the query tokenizer.
assertQueryTests :: String -> TestPairSuite -> Assertion
assertQueryTests descr = mapM_ (uncurry (assertQuery descr))


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
                  , ("abc 123 4H5", ["abc", "123", "4h5"])
                  , ("abc%1234!!fdjs", ["abc", "1234", "fdjs"])
                  , ("can't won't haven't Bobby's", ["can't", "won't", "haven't", "bobby's"])
                  , ("high-five low-five ice-cream", ["high-five", "low-five", "ice-cream"])
                  , ("what--dash more---dashing!", ["what--dash", "more---dashing"])
                  , ("a1b2 ab12cd34", ["a1b2", "ab12cd34"])
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


assertQuerySimple :: Assertion
assertQuerySimple = assertQueryTests "assertQuerySimple" tests
    where tests = [ ("b", ["b"])
                  , ("x", ["x"])
                  , ("X", ["x"])
                  , ("1", ["1"])
                  , ("8", ["8"])
                  , ("'", [])
                  , ("%", [])
                  , ("\n", [])
                  , (" ", [])
                  , ("*", [])
                  , ("abcdef", ["abcdef"])
                  , ("4321", ["4321"])
                  ]

assertQueryMultiple :: Assertion
assertQueryMultiple = assertQueryTests "assertQueryMultiple" tests
    where tests = [ ("b c d", ["b", "c", "d"])
                  , ("E f G", ["e", "f", "g"])
                  , ("H\tJ\nK", ["h", "j", "k"])
                  , ("abc DeF gHi", ["abc", "def", "ghi"])
                  , ("1 2 3", ["1", "2", "3"])
                  , ("abc 123 4H5", ["abc", "123", "4h5"])
                  , ("abc%1234!!fdjs", ["abc", "1234", "fdjs"])
                  , ("can't won't haven't Bobby's", ["can't", "won't", "haven't", "bobby's"])
                  , ("high-five low-five ice-cream", ["high-five", "low-five", "ice-cream"])
                  , ("what--dash more---dashing!", ["what--dash", "more---dashing"])
                  , ("a1b2 ab12cd34", ["a1b2", "ab12cd34"])
                  , ("something * else", ["something", "else"])
                  ]

assertQueryWildcards :: Assertion
assertQueryWildcards = assertQueryTests "assertQueryWildcards" tests
    where tests = [ ("b *c d", ["b", "*c", "d"])
                  , ("E f* G", ["e", "f*", "g"])
                  , ("H\tJ\nK", ["h", "j", "k"])
                  , ("abc *DeF* gHi", ["abc", "*def*", "ghi"])
                  , ("1 2 3", ["1", "2", "3"])
                  , ("abc 123 4*H5", ["abc", "123", "4*h5"])
                  , ("a1b2 * *ab12*cd34*", ["a1b2", "*ab12*cd34*"])
                  , ("something * else", ["something", "else"])
                  ]

tokenizerTests :: [Test]
tokenizerTests =
    [ testGroup "tokenizer" [ testCase "simple" assertTokenSimple
                            , testCase "multiple" assertTokenMultiple
                            , testCase "stop-list" assertTokenStopListed
                            ]
    , testGroup "queries"   [ testCase "simple" assertQuerySimple
                            , testCase "multiple" assertQueryMultiple
                            , testCase "wildcards" assertQueryWildcards
                            ]
    ]

