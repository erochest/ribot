
-- This contains the tokenizer for search and generating the MM.

module Text.Ribot.Tokenizer
    ( -- String
    -- , [String]
      tokenize
    -- , isWord
    -- , removeWords
    -- , stopList
    -- , removeStopList
    ) where

import qualified Data.Char as C
import           Text.ParserCombinators.Parsec


-- First, let's define some type aliases to make things more clear below.
-- type Token     = String
-- type TokenList = [String]

-- This is the main function. It breaks a `String` into a `[String]`. It uses
-- a Parsec parser combinator to do this. The input should be very short, so we
-- don't need to worry about handling large input or lazy evaluation or
-- anything.
--
-- The two parameters are the source (channel + nick, maybe) and the line to
-- parse.
tokenize :: String -> String -> Either ParseError [String]
tokenize = parse tokenList

-- A `tokenList` is optional whitespace followed by a list of tokens, separated
-- by and optionally ended by whitespace.
tokenList :: GenParser Char st [String]
tokenList = spaces >> singleToken `sepEndBy` (skipMany1 space <|> eof)

-- A `singleToken` is a punctuation character or a `word`.
singleToken :: GenParser Char st String
singleToken =   (satisfy isPunctuation >>= \c -> return [c])
            <|> word

-- A `word` is one or more alpha-numeric characters, optionally followed by a
-- punctuation mark immediately followed by another word (alphanumber and
-- another suffix).
word :: GenParser Char st String
word = do
    base <- many1 alphaNum
    rest <- optionMaybe suffix
    let normBase = map C.toLower base
    return $ case rest of
        Just rest' -> normBase ++ rest'
        Nothing    -> normBase

suffix :: GenParser Char st String
suffix = do
    p <- satisfy isPunctuation
    rest <- word
    return (p:rest)

-- This is my re-definition of `C.isPunctuation` so that it includes symbols.
isPunctuation :: Char -> Bool
isPunctuation c | C.isPunctuation c = True
isPunctuation c | C.isSymbol c      = True
isPunctuation _                     = False

