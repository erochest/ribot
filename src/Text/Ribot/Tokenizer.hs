
-- This contains the tokenizer for search and generating the MM.

module Text.Ribot.Tokenizer
    ( tokenize
    , isWord
    , removePunctuation
    , stopList
    , inStopList
    , removeStopWords
    ) where

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Set as S
import           Text.ParserCombinators.Parsec


-- This is an English stop list taken from http://www.nltk.org/.
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
tokenList = do
    spaces
    (eof >> return []) <|> tokenRest

-- This is a token (immediate, no leading whitespace) and the rest of the
-- tokens.
tokenRest :: GenParser Char st [String]
tokenRest = do
    t <- singleToken
    ts <- tokenList
    return $ t:ts

-- A `singleToken` is a punctuation character or a `word`.
singleToken :: GenParser Char st String
singleToken =   (punctuation >>= \c -> return [c])
            <|> word

-- A `word` is one or more alpha-numeric characters, optionally followed by a
-- punctuation mark immediately followed by another word (alphanumber and
-- another suffix).
word :: GenParser Char st String
word = do
    base <- many1 alphaNum
    rest <- optionMaybe $ try suffix
    let normBase = map C.toLower base
    return $ case rest of
        Just rest' -> normBase ++ rest'
        Nothing    -> normBase

-- This is a suffix of a word. It contains one or more punctuation marks
-- followed by another word.
suffix :: GenParser Char st String
suffix = do
    p <- many1 punctuation
    rest <- word
    return $ p ++ rest

-- This is a single punctuation character.
punctuation :: GenParser Char st Char
punctuation = satisfy isPunctuation

-- This is my re-definition of `C.isPunctuation` so that it includes symbols.
isPunctuation :: Char -> Bool
isPunctuation c | C.isPunctuation c = True
isPunctuation c | C.isSymbol c      = True
isPunctuation _                     = False

