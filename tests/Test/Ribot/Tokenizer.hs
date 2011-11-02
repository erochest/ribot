
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
    assertBool ("Whitespace returns no tokens.: " ++ (show tokens))
               ([] == tokens)
    where tokens = onlyRight [] $ tokenize "<assertWhiteSpace>" "   \t\n \t"

assertSingleWord :: Assertion
assertSingleWord =
    assertBool ("Single word: " ++ (show tokens))
               (["word"] == tokens)
    where tokens = onlyRight [] $ tokenize "<assertSingleWord>" "word"

assertAlphaNum :: Assertion
assertAlphaNum =
    assertBool ("Alpha-numeric tokens are returned.: " ++ (show tokens))
               (["alpha", "numeric", "tokens", "are", "returned"] == tokens)
    where tokens = onlyRight [] $ tokenize "<assertAlphaNum>" "alpha numeric tokens\nare\treturned"

assertNormalized :: Assertion
assertNormalized =
    assertBool ("Alpha-numeric characters are normalized to lower-case.: " ++ (show tokens))
               (["alpha", "numeric", "characters", "are", "normalized", "to", "lower", "case"] ==
                tokens)
    where tokens = onlyRight [] $ tokenize "<assertNormalized>" "AlPhA NuMeRiC ChArAcTeRs aRe nOrMaLiZeD To lOwEr cAsE"

assertLeadingWhiteSpace :: Assertion
assertLeadingWhiteSpace =
    assertBool ("Leading whitespace is skipped.: " ++ (show tokens))
               (["leading", "whitespace", "is", "skipped"] == tokens)
    where tokens = onlyRight [] $ tokenize "<assertLeadingWhiteSpace>" "  \t  \n  Leading whitespace is skipped"

assertTrailingWhiteSpace :: Assertion
assertTrailingWhiteSpace =
    assertBool ("Trailing whitespace is skipped.: " ++ (show tokens))
               (["trailing", "whitespace", "is", "skipped"] == tokens)
    where tokens = onlyRight [] $ tokenize "<assertTrailingWhiteSpace>" "Trailing whitespace is skipped  \t  \n  "

assertPunctuation :: Assertion
assertPunctuation =
    assertBool ("Punctuation is returned as a token.: " ++ (show tokens))
               ([".", ",", ":", "\"", "$"] == tokens)
    where tokens = onlyRight [] $ tokenize "<assertPunctuation>" ". , : \" $"

assertSinglePunctuation :: Assertion
assertSinglePunctuation =
    assertBool ("Punctuation marks are returned singly.: " ++ (show tokens))
               ([".", ",", ":", "\"", "$"] == tokens)
    where tokens = onlyRight [] $ tokenize "<assertSinglePunctuation>" ".,:\"$"

assertLeadingPunctuation :: Assertion
assertLeadingPunctuation =
    assertBool ("Punctuation marks prefixing words create their own tokens.: " ++ (show tokens))
               (["\"", "'", "this", "!", "s", "a", "?", "quote"] == tokens)
    where tokens = onlyRight [] $ tokenize "<assertLeadingPunctuation>" "\"'This !s a ?quote"

assertTrailingPunctuation :: Assertion
assertTrailingPunctuation =
    assertBool ("Punctuation marks suffixing words create their own tokens.: " ++ (show tokens))
               (["this", ",", "is", ":", "a", "-", "quote", ".", "'", "\""] == tokens)
    where tokens = onlyRight [] $ tokenize "<assertTrailingPunctuation>" "This, is: a- quote.'\""

assertContraction :: Assertion
assertContraction =
    assertBool ("Contractions are included with the main token.: " ++ (show tokens))
               (["can't", "won't", "didn't", "bobby's"] == tokens)
    where tokens = onlyRight [] $ tokenize "<assertContraction>" "Can't won't didn't Bobby's"

assertNumbers :: Assertion
assertNumbers =
    assertBool ("Commas and decimals in numbers don't split the numbers up.: " ++ (show tokens))
               (["1,000.00", "3.1415", "1,234,567"] == tokens)
    where tokens = onlyRight [] $ tokenize "<assertNumbers>" "1,000.00 3.1415 1,234,567"

assertUrl :: Assertion
assertUrl =
    assertBool ("URLs are their own tokens.: " ++ (show tokens))
               (["http://www.google.com", "www.scholarslab.org", "/", "erochest@gmail.com"] ==
                tokens)
    where tokens = onlyRight [] $ tokenize "<assertUrl>" "http://www.google.com www.scholarslab.org/ erochest@gmail.com"

assertIsWord :: Assertion
assertIsWord = do
    assertBool "Alpha-numeric are words." $ isWord "Hi"
    assertBool "Numeric are words." $ isWord "42"
    assertBool "Punctuation aren't words." $ not (isWord ".")
    assertBool "Empty isn't a word." $ not (isWord "")

assertRemovePunctuation :: Assertion
assertRemovePunctuation =
    assertBool ("Non-word tokens are removed: " ++ (show tokens))
               (["this", "is", "a", "quote"] == tokens)
    where tokens = removePunctuation
                 . onlyRight []
                 $ tokenize "<assertTrailingPunctuation>" "This, is: a- quote.'\""


assertInStopList :: Assertion
assertInStopList = do
    assertBool "'the' is in the stoplist." $ inStopList "the"
    assertBool "'into' is in the stoplist." $ inStopList "into"
    assertBool "'assertion' is not in the stoplist. " $ not (inStopList "assertion")

assertRemoveStopWords :: Assertion
assertRemoveStopWords = do
    assertBool ("'the' is in the stoplist. => " ++ (show tokens))
               (["'", "'", "stoplist", "."] == tokens)
    where tokens = removeStopWords
                 . onlyRight []
                 $ tokenize "<assertRemoveStopWords>" "'the' is in the stoplist."

tokenizerTests :: [Test]
tokenizerTests =
    [ testGroup "tokenizer" [ testCase "whitespace" assertWhiteSpace
                            , testCase "single-word" assertSingleWord
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
    , testGroup "clean-up"  [ testCase "is-word" assertIsWord
                            , testCase "remove-punctuation" assertRemovePunctuation
                            ]
    , testGroup "stop-list" [ testCase "in-stoplist" assertInStopList
                            , testCase "remove-stopwords" assertRemoveStopWords
                            ]
    ]

