
-- This contains the text- and parsing-related aspects of searching.

module Text.Ribot.Search
    ( parseSearch
    , tokenize
    ) where

import qualified Data.Char as C
import qualified Data.List as L
import           Text.ParserCombinators.Parsec
-- [Text.Ribot.Tokenizer](Tokenizer.html)
import qualified Text.Ribot.Tokenizer as T

-- This takes an input string and tokenizes it and removes stop words and
-- punctuation. The parameters are the user's nick and the message.
tokenize :: String -> String -> [String]
tokenize nick input = 
    case T.tokenize nick input of
        Left _          -> []
        Right allTokens -> L.filter keep allTokens
    where
        keep :: String -> Bool
        keep word = T.isWord word && not (T.inStopList word)

-- Parse search parses the search query according to these rules:
--
-- * for the most part, tokenization follows `tokenize` above;
-- * English stop words and punctuation are stripped out; and
-- * wildcards (stars) are turned into SQL wildcards (*%*).
parseSearch :: String -> [String]
parseSearch input =
    case tokenizeQuery "<query>" input of
        Left _          -> []
        Right allTokens -> map normalize $ L.filter keep allTokens
    where
        -- This handles interacting with the parser.
        tokenizeQuery :: String -> String -> Either ParseError [String]
        tokenizeQuery = parse tokenList

        -- This is a predicate defining the tokens that need to be kept.
        keep :: String -> Bool
        keep = not . T.inStopList

        -- This fixes the wildcard characters in a string and lower-cases the
        -- characters.
        normalize :: String -> String
        normalize []       = []
        normalize ('*':xs) = '%' : normalize xs
        normalize (x:xs)   = C.toLower x : normalize xs

        -- This defines a list of tokens.
        tokenList :: GenParser Char st [String]
        tokenList = word `sepBy` many (satisfy isJunk)

        -- A single token. This is where it differs from
        -- [`Text.Ribot.Tokenizer`](Tokenizer.html).
        word :: GenParser Char st String
        word = do
            first <- alphaNum <|> char '*'
            rest <- many (alphaNum <|> char '*' <|> oneOf ".")
            return (first:rest)

        -- This is a predicate defining a junk character. It's basically a
        -- predicate inverse of `word`.
        isJunk :: Char -> Bool
        isJunk '*'                = False
        isJunk c | C.isAlphaNum c = False
        isJunk _                  = True

