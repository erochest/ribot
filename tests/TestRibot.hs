

module Main where

import Test.Ribot.Tokenizer

import Test.Framework (Test, defaultMain, testGroup)

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
    [ testGroup "tokenizer" tokenizerTests
    ]

