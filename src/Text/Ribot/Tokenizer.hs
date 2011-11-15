{-# LANGUAGE FlexibleContexts #-}

-- This contains the tokenizer for search and generating the MM.

module Text.Ribot.Tokenizer
    ( lex
    , token
    , Lex(..)

    , tokenize
    , isWord
    , removePunctuation
    , stopList
    , inStopList
    , removeStopWords
    ) where

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Set as S
import           Text.Parsec hiding (token, try)
import           Text.ParserCombinators.Parsec hiding (token)
import           Prelude hiding (lex)


-- This provides the data definition for the lexed text.
data Lex = LexAlphaNum String
         | LexWS String
         | LexInterToken Char
         | LexPunct Char
    deriving (Show, Eq)

-- This breaks a text string into a list of `Lex` data that the tokenizer can
-- throw away, join to other lexed items, or output them as is.
--
-- The parameters are the source and the line to lex.
lex :: String -> String -> Either ParseError [Lex]
lex = parse lexItems

-- A list of lexed items.
lexItems :: GenParser Char st [Lex]
lexItems = many (   lexAlphaNum
                <|> lexWS
                <|> lexInterToken
                <|> lexPunct
                )

-- A string of alpha-numeric characters.
lexAlphaNum :: GenParser Char st Lex
lexAlphaNum = many1 alphaNum >>= return . LexAlphaNum

-- A string of whitespace characters.
lexWS :: GenParser Char st Lex
lexWS = many1 space >>= return . LexWS

-- A single inter-token punctuation character.
lexInterToken :: GenParser Char st Lex
lexInterToken = oneOf "-.,'" >>= return . LexInterToken

-- A single punctuation character.
lexPunct :: GenParser Char st Lex
lexPunct = anyChar >>= return . LexPunct

-- This processes the output of `lex`. It does one of several things:
--
-- * drop trash and whitespace;
-- * put word tokens with intra-token punctuation together; and
-- * pass simple word tokens through.
--
-- This also normalizes all tokens.
token :: String -> [Lex] -> Either ParseError [String]
token = parse tokenList

-- This is a list of tokens.
tokenList :: GenParser Lex st [String]
tokenList = trash >> many tokenItem

-- This is a parser combinator that passes anything that matches a predicate.
tokenp :: Stream s m Lex => (Lex -> Bool) -> ParsecT s u m Lex
tokenp f = tokenPrim (\l -> "'" ++ (show l) ++ "'")
                     nextPos
                     (\l -> if f l then Just l else Nothing)
    where
        nextPos pos _ _ = incSourceColumn pos 1

-- This is a single token.
tokenItem :: GenParser Lex st String
tokenItem = do
    token <- tokenword
    rest  <- many (try tokenItem')
    trash
    return . L.concat $ (token : rest)

-- This defines the word part of a token (i.e., the alpha-numeric part).
tokenword :: GenParser Lex st String
tokenword = tokenp isLexAlphaNum >>= return . lexToString
    where
        isLexAlphaNum :: Lex -> Bool
        isLexAlphaNum (LexAlphaNum _) = True
        isLexAlphaNum _               = False

-- This is a sub-function of `tokenItem`. It handles the rest of a token,
-- everything after the initial word-part.
tokenItem' :: GenParser Lex st String
tokenItem' = do
    p <- tokenintrapunct
    w <- tokenword
    return $ p ++ w

-- This defines any characters that are valid trash between tokens.
trash :: GenParser Lex st ()
trash = skipMany (tokenpunct <|> tokenws <|> tokenintrapunct)

-- This is a string of whitespace.
tokenws :: GenParser Lex st String
tokenws = tokenp isWS >>= return . lexToString
    where
        isWS :: Lex -> Bool
        isWS (LexWS _) = True
        isWS _         = False

-- This is an intra-token punctuation.
tokenintrapunct :: GenParser Lex st String
tokenintrapunct = tokenp isIntraTokenPunct >>= return . lexToString
    where
        isIntraTokenPunct :: Lex -> Bool
        isIntraTokenPunct (LexInterToken _) = True
        isIntraTokenPunct _                 = False

-- This is punctuation that must happen around tokens.
tokenpunct :: GenParser Lex st String
tokenpunct = tokenp isTokenPunct >>= return . lexToString
    where
        isTokenPunct :: Lex -> Bool
        isTokenPunct (LexPunct _) = True
        isTokenPunct _            = False

-- This takes a `Lex` instance and turns it into a string.
lexToString :: Lex -> String
lexToString (LexAlphaNum l)   = l
lexToString (LexWS l)         = l
lexToString (LexInterToken l) = [l]
lexToString (LexPunct l)      = [l]

-- This lexes a string, tokenizes it, and normalizes the output by converting
-- it to lower-case.
tokenize :: String -> String -> Either ParseError [String]
tokenize src input =   lex src input
                   >>= token src
                   >>= return . map normalize
    where
        normalize :: String -> String
        normalize = map C.toLower


-- This is an English stop list taken from the [Natural Language
-- Toolkit](http://www.nltk.org/).
stopList :: S.Set String
stopList = S.fromList [ "i", "me", "my", "myself", "we", "our", "ours",
    "ourselves", "you", "your", "yours", "yourself", "yourselves", "he", "him",
    "his", "himself", "she", "her", "hers", "herself", "it", "its", "itself",
    "they", "them", "their", "theirs", "themselves", "what", "which", "who",
    "whom", "this", "that", "these", "those", "am", "is", "are", "was", "were",
    "be", "been", "being", "have", "has", "had", "having", "do", "does", "did",
    "doing", "a", "an", "the", "and", "but", "if", "or", "because", "as",
    "until", "while", "of", "at", "by", "for", "with", "about", "against",
    "between", "into", "through", "during", "before", "after", "above",
    "below", "to", "from", "up", "down", "in", "out", "on", "off", "over",
    "under", "again", "further", "then", "once", "here", "there", "when",
    "where", "why", "how", "all", "any", "both", "each", "few", "more", "most",
    "other", "some", "such", "no", "nor", "not", "only", "own", "same", "so",
    "than", "too", "very", "s", "t", "can", "will", "just", "don", "should",
    "now" ]

-- This tests whether a string is a word in the stoplist.
inStopList :: String -> Bool
inStopList = (flip S.member) stopList

-- This removes all words in the stop list from the list of tokens.
removeStopWords :: [String] -> [String]
removeStopWords = L.filter (not . inStopList)

-- This tests whether a string is a word by looking at its first character.
isWord :: String -> Bool
isWord (c:_) | C.isAlphaNum c = True
isWord _                      = False

-- This removes all punctuation tokens from a list of tokens.
removePunctuation :: [String] -> [String]
removePunctuation = L.filter isWord

-- This is my re-definition of `C.isPunctuation` so that it includes symbols.
isPunctuation :: Char -> Bool
isPunctuation c | C.isPunctuation c = True
isPunctuation c | C.isSymbol c      = True
isPunctuation _                     = False

