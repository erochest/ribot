
module Test.Ribot.Tokenizer (tokenizerTests) where

import           Test.HUnit (Assertion, assertBool)
import           Test.Framework (Test, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Text.Ribot.Tokenizer


onlyRight :: b -> Either a b -> b
onlyRight def (Left _)    = def
onlyRight _   (Right val) = val


assertWhiteSpace :: Assertion
assertWhiteSpace =
    assertBool "Whitespace returns no tokens."
               ([] == tokens)
    where tokens = onlyRight [] $ tokenize "<assertWhiteSpace>" "   \t\n \t"

assertAlphaNum :: Assertion
assertAlphaNum =
    assertBool "Alpha-numeric tokens are returned."
               (["alpha", "numeric", "tokens", "are", "returned"] == tokens)
    where tokens = onlyRight [] $ tokenize "<assertAlphaNum>" "alpha numeric tokens\nare\treturned"

assertNormalized :: Assertion
assertNormalized =
    assertBool "Alpha-numeric characters are normalized to lower-case."
               (["alpha", "numeric", "characters", "are", "normalized", "to", "lower", "case"] ==
                tokens)
    where tokens = onlyRight [] $ tokenize "<assertNormalized>" "AlPhA NuMeRiC ChArAcTeRs aRe nOrMaLiZeD To lOwEr cAsE"

assertLeadingWhiteSpace :: Assertion
assertLeadingWhiteSpace =
    assertBool "Leading whitespace is skipped."
               (["leading", "whitespace", "is", "skipped"] == tokens)
    where tokens = onlyRight [] $ tokenize "<assertLeadingWhiteSpace>" "  \t  \n  Leading whitespace is skipped"

assertTrailingWhiteSpace :: Assertion
assertTrailingWhiteSpace =
    assertBool "Trailing whitespace is skipped."
               (["trailing", "whitespace", "is", "skipped"] == tokens)
    where tokens = onlyRight [] $ tokenize "<assertTrailingWhiteSpace>" "Trailing whitespace is skipped  \t  \n  "

assertPunctuation :: Assertion
assertPunctuation =
    assertBool "Punctuation is returned as a token."
               ([".", ",", ":", "\"", "$"] == tokens)
    where tokens = onlyRight [] $ tokenize "<assertPunctuation>" ". , : \" $"

assertSinglePunctuation :: Assertion
assertSinglePunctuation =
    assertBool "Punctuation marks are returned singly."
               ([".", ",", ":", "\"", "$"] == tokens)
    where tokens = onlyRight [] $ tokenize "<assertSinglePunctuation>" ".,:\"$"

assertLeadingPunctuation :: Assertion
assertLeadingPunctuation =
    assertBool "Punctuation marks prefixing words create their own tokens."
               (["\"", "'", "this", "!", "s", "a", "?", "quote"] == tokens)
    where tokens = onlyRight [] $ tokenize "<assertLeadingPunctuation>" "\"'This !s a ?quote"

assertTrailingPunctuation :: Assertion
assertTrailingPunctuation =
    assertBool "Punctuation marks suffixing words create their own tokens."
               (["this", ",", "is", ":", "a", "-", "quote", ".", "'", "\""] == tokens)
    where tokens = onlyRight [] $ tokenize "<assertTrailingPunctuation>" "This, is: a- quote.'\""

assertContraction :: Assertion
assertContraction =
    assertBool "Contractions are included with the main token."
               (["can't", "won't", "didn't", "bobby's"] == tokens)
    where tokens = onlyRight [] $ tokenize "<assertContraction>" "Can't won't didn't Bobby's"

assertNumbers :: Assertion
assertNumbers =
    assertBool "Commas and decimals in numbers don't split the numbers up."
               (["1,000.00", "3.1415", "1,234,567"] == tokens)
    where tokens = onlyRight [] $ tokenize "<assertNumbers>" "1,000.00 3.1415 1,234,567"

assertUrl :: Assertion
assertUrl =
    assertBool "URLs are their own tokens."
               (["http://www.google.com", "www.scholarslab.org", "/", "erochest@gmail.com"] ==
                tokens)
    where tokens = onlyRight [] $ tokenize "<assertUrl>" "http://www.google.com www.scholarslab.org/ erochest@gmail.com"


tokenizerTests :: [Test]
tokenizerTests =
    [ testGroup "tokenizer" [ testCase "whitespace" assertWhiteSpace
                            , testCase "alpha-numeric" assertAlphaNum
                            , testCase "normalized" assertNormalized
                            , testCase "leading-whitespace" assertLeadingWhiteSpace
                            , testCase "trailing-whitespace" assertTrailingWhiteSpace
                            , testCase "punctuation" assertPunctuation
                            , testCase "single-punctuation" assertSinglePunctuation
                            , testCase "leading-punctuation" assertLeadingPunctuation
                            , testCase "trailing-punctuation" assertTrailingPunctuation
                            , testCase "contraction" assertContraction
                            , testCase "numbers" assertNumbers
                            , testCase "url" assertUrl
                            ]
    ]

