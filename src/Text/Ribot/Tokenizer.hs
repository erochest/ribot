{-# LANGUAGE OverloadedStrings #-}

-- This extends the bakers12 tokenizer to provide a vaguely English-like
-- tokenizer.

module Text.Ribot.Tokenizer
    ( tokenize
    , alphaNumFilter
    , stopList
    , inStopList
    , stopListFilter
    ) where

import           Control.Exception (SomeException)
import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL
import qualified Data.Set as S
import qualified Data.Text as T
import           Network.IRC.Commands (Channel)
import qualified Text.Bakers12.Tokenizer as B12
import           Text.Bakers12.Tokenizer.Types hiding (append, concat)

-- This tokenizes a string into a list of tokens.
tokenize :: Channel -> T.Text -> Either SomeException [Token]
tokenize channel input = E.runLists [[input]] process
    where process =      B12.tokenizeStream channel 0
                    E.=$ alphaNumFilter
                    E.=$ stopListFilter
                    E.=$ EL.consume

-- This filters out anything that's not alphnumeric.
alphaNumFilter :: Monad m => E.Enumeratee Token Token m b
alphaNumFilter = EL.filter (predicate . tokenType)
    where predicate ttype = ttype == AlphaToken || ttype == NumberToken

-- This is an English stop list taken from the [Natural Language
-- Toolkit](http://www.nltk.org/).
stopList :: S.Set T.Text
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

-- This is a simple predicate testing whether a string is in the stop list.
inStopList :: T.Text -> Bool
inStopList = flip S.member stopList

-- This is a simple enumeratee that filters any stop-listed words.
stopListFilter :: Monad m => E.Enumeratee Token Token m b
stopListFilter = EL.filter (not . inStopList . tokenText)


