
module Test.Ribot.Tokenizer (tokenizerTests) where

import           Test.HUnit (Assertion, assertBool)
import           Test.Framework (Test, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Text.Ribot.Tokenizer
import           Prelude hiding (lex)


onlyRight :: b -> Either a b -> b
onlyRight def (Left _)    = def
onlyRight _   (Right val) = val


assertLex :: String -> String -> [Lex] -> Assertion
assertLex descr input expected =
    assertBool (descr ++ ": " ++ (show input) ++ " => " ++ (show lexes))
               (expected == lexes)
    where lexes = onlyRight [] $ lex "" input

assertLexAlphaNum :: Assertion
assertLexAlphaNum = do
    assertLex descr "a" [LexAlphaNum "a"]
    assertLex descr "x" [LexAlphaNum "x"]
    assertLex descr "M" [LexAlphaNum "M"]
    assertLex descr "3" [LexAlphaNum "3"]
    assertLex descr "0" [LexAlphaNum "0"]
    assertLex descr "abc" [LexAlphaNum "abc"]
    assertLex descr "XyZZy" [LexAlphaNum "XyZZy"]
    assertLex descr "31415" [LexAlphaNum "31415"]
    assertLex descr "fdsa90890" [LexAlphaNum "fdsa90890"]
    assertLex descr "3388aa77zz" [LexAlphaNum "3388aa77zz"]
    where descr = "Lex alphanumeric"

assertLexWS :: Assertion
assertLexWS = do
    assertLex descr " " [LexWS " "]
    assertLex descr "\t" [LexWS "\t"]
    assertLex descr "\r" [LexWS "\r"]
    assertLex descr "   " [LexWS "   "]
    assertLex descr "   \t  " [LexWS "   \t  "]
    where descr = "Lex whitespace"

assertLexInterTokenP :: Assertion
assertLexInterTokenP = do
    assertLex descr "-" [LexInterToken '-']
    assertLex descr "." [LexInterToken '.']
    assertLex descr "," [LexInterToken ',']
    assertLex descr "'" [LexInterToken '\'']
    assertLex descr "',.-" [ LexInterToken '\''
                           , LexInterToken ','
                           , LexInterToken '.'
                           , LexInterToken '-'
                           ]
    where descr = "Lex inter-token punctuation"

assertLexCombined :: Assertion
assertLexCombined = do
    assertLex descr "a " [LexAlphaNum "a", LexWS " "]
    assertLex descr " a" [LexWS " ", LexAlphaNum "a"]
    assertLex descr "   aaa" [LexWS "   ", LexAlphaNum "aaa"]
    assertLex descr "a b\tc\nd" [ LexAlphaNum "a"
                                , LexWS " "
                                , LexAlphaNum "b"
                                , LexWS "\t"
                                , LexAlphaNum "c"
                                , LexWS "\n"
                                , LexAlphaNum "d"
                                ]
    assertLex descr "a-" [LexAlphaNum "a", LexInterToken '-']
    assertLex descr " . " [LexWS " ", LexInterToken '.', LexWS " "]
    assertLex descr "a, " [LexAlphaNum "a", LexInterToken ',', LexWS " "]
    where descr = "Lex combined"

tokenizerTests :: [Test]
tokenizerTests =
    [ testGroup "lexer" [ testCase "alpha-numeric" assertLexAlphaNum
                        , testCase "whitespace" assertLexWS
                        , testCase "inter-token punctuation" assertLexInterTokenP
                        , testCase "combined" assertLexCombined
                        ]
    ]

