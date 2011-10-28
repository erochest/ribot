
-- This contains the tokenizer for search and generating the MM.

module Text.Ribot.Tokenizer
    ( Token
    , TokenList
    , tokenize
    , isWord
    , removeWords
    , stopList
    , removeStopList
    ) where

import qualified Data.Char as C
import           Text.ParserCombinators.Parsec


-- First, let's define some type aliases to make things more clear below.
type Token     = String
type TokenList = [String]

-- This is the main function. It breaks a `String` into a `TokenList`. It uses
-- a Parsec parser combinator to do this. The input should be very short, so we
-- don't need to worry about handling large input or lazy evaluation or
-- anything.
--
-- The two parameters are the source (channel + nick, maybe) and the line to
-- parse.
tokenize :: String -> String -> Either ParseError TokenList
tokenize = parse tokenList

-- A `tokenList` is optional whitespace followed by a list of tokens, separated
-- by and optionally ended by whitespace.
tokenList = spaces >> token `sepEndBy` (many1 space <|> eof)

-- A `token` is a punctuation character or a `word`.
token =   satisfy C.isPunctuation
      <|> word

-- A `word` is one or more alpha-numeric characters, optionally followed by a
-- punctuation mark immediately followed by another word (alphanumber and
-- another suffix).
word =
    many1 alphaNum
    optional (satisfy C.isPunctuation >> word)


