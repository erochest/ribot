{-# LANGUAGE OverloadedStrings #-}

-- This extends the bakers12 tokenizer to provide a vaguely English-like
-- tokenizer.

module Text.Ribot.Tokenizer
    ( tokenize
    , tokenizeQuery
    , getTokenText
    , alphaNumFilter
    , stopList
    , inStopList
    , stopListFilter
    ) where

import           Control.Exception (SomeException)
import           Control.Monad.Trans (lift)
import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL
import           Data.Maybe (isJust, isNothing)
import qualified Data.Set as S
import qualified Data.Text as T
import           Network.IRC.Commands (Channel)
import qualified Text.Bakers12.Tokenizer as B12
import           Text.Bakers12.Tokenizer.Types hiding (append, concat)
import qualified Text.Bakers12.Tokenizer.Types as BT12

-- This tokenizes a string into a list of tokens.
tokenize :: Channel -> T.Text -> Either SomeException [Token]
tokenize channel input = E.runLists [[input]] process
    where process =      B12.tokenizeStream channel 0
                    E.=$ (combineRuns isAlphaNum)
                    E.=$ (join isAlphaNum isSingleQuote)
                    E.=$ alphaNumFilter
                    E.=$ stopListFilter
                    E.=$ EL.consume

-- This tokenizes a search query by concatenating wildcards in with the
-- adjacent word or number.
tokenizeQuery :: Channel -> T.Text -> Either SomeException [Token]
tokenizeQuery _ _ = Right []

-- This converts the output of a tokenize* function into a list of `Text`.
-- Exceptions are silently turned into empty lists.
getTokenText :: Either SomeException [Token] -> [T.Text]
getTokenText (Left _)       = []
getTokenText (Right tokens) = map tokenText tokens

-- This filters out anything that's not alphnumeric.
alphaNumFilter :: Monad m => E.Enumeratee Token Token m b
alphaNumFilter = EL.filter isAlphaNum

-- This is the alphanumeric predicate.
isAlphaNum :: Token -> Bool
isAlphaNum (Token _ _ _ AlphaToken  _ _) = True
isAlphaNum (Token _ _ _ NumberToken _ _) = True
isAlphaNum _                             = False

-- This tests for a single quote.
isSingleQuote :: Token -> Bool
isSingleQuote (Token "'" _ _ PunctuationToken _ _) = True
isSingleQuote _                                    = False

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

-- This combines runs of tokens that satisfy a predicate.
combineRuns :: Monad m => (Token -> Bool) -> E.Enumeratee Token Token m b
combineRuns predicate (E.Continue k) = do
    toCombine' <- EL.takeWhile predicate
    case toCombine' of
        [] -> do
            next' <- EL.head
            case next' of
                Nothing -> return $ E.Continue k
                Just next  -> do
                    newStep <- lift $ E.runIteratee $ k $ E.Chunks [next]
                    combineRuns predicate newStep
        toCombine -> do
            newStep <- lift $ E.runIteratee $ k $ E.Chunks [BT12.concat toCombine]
            combineRuns predicate newStep
combineRuns _ step = return step

-- This joins triples of tokens where the first and last match the initial
-- predicate and the joiner matches the second.
join :: Monad m => (Token -> Bool) -> (Token -> Bool) -> E.Enumeratee Token Token m b
join itemp conjp (E.Continue k) =
    step itemp (return $ E.Continue k) (\t0 -> next [t0]) $ \t0 ->
        step conjp (next [t0]) (\t1 -> next [t0, t1]) $ \t1 ->
            step itemp (next [t0, t1]) (\t2 -> next [t0, t1, t2]) $ \t2 ->
                next [BT12.concat [t0, t1, t2]]

    where
        next chunk = do
            next' <- lift $ E.runIteratee $ k $ E.Chunks chunk
            join itemp conjp next'

        step predicate onNothing onFalse onTrue = do
            item' <- EL.head
            case item' of
                Nothing   -> onNothing
                Just item -> if predicate item
                             then onTrue item
                             else onFalse item
join _ _ step = return step

-- This is the state used to combine tokens. It has to track the last seen
-- token and the tokens seen for the base, connector, and tail.
data CombineState = CombineState
    { tokenBase      :: [Token]
    , tokenConnector :: Maybe Token
    , tokenTail      :: Maybe Token
    }

nullCombine :: CombineState
nullCombine = CombineState [] Nothing Nothing

-- This pushes token onto some part of the state to produce the next state.
pushBase :: CombineState -> Token -> CombineState
pushBase state token = state { tokenBase = (token:tokenBase state) }

pushConnector :: CombineState -> Token -> CombineState
pushConnector state token = state { tokenConnector = Just token }

pushTail :: CombineState -> Token -> CombineState
pushTail state token = state { tokenTail = Just token }

-- This is an enumeratee that combines the input tokens that need to be
-- combined into one token.
--
-- * Contractions (ALPHA - SINGLE QUOTE - ALPHA)
combineTokenParts :: Monad m => E.Enumeratee Token Token m b
combineTokenParts = EL.concatMapAccum combine nullCombine

-- This is the function that handles each input token and accumulates it with
-- the existing state, optionally producing output.
combine :: CombineState -> Token -> (CombineState, [Token])
combine state@(CombineState base connector tail) token
    -- Trash
    | tokenType token == SeparatorToken = (nullCombine, combineOutput state)
    | tokenType token == SymbolToken    = (nullCombine, combineOutput state)
    | tokenType token == MarkToken      = (nullCombine, combineOutput state)
    | tokenType token == UnknownToken   = (nullCombine, combineOutput state)

    -- Numbers (flush buffer, output current token, and start over)
    | tokenType token == NumberToken    =
        (nullCombine, combineOutput state ++ [token])

    -- Letters
    | tokenType token == AlphaToken && isNothing connector =
        (pushBase state token, [])
    | tokenType token == AlphaToken && isJust connector    =
        (nullCombine, combineOutput $ pushTail state token)

    -- Punctuation (some of it's trash)
    | tokenType token == PunctuationToken && tokenText token == "\'" && isNothing connector =
        (pushConnector state token, [])
    | tokenType token == PunctuationToken && tokenText token == "\'" && isJust connector =
        (nullCombine, combineOutput state)
    | tokenType token == PunctuationToken =
        (nullCombine, combineOutput state)
        

-- This takes the combine state and produces output.
combineOutput :: CombineState -> [Token]
combineOutput (CombineState []   _        _       ) = []
combineOutput (CombineState base Nothing  _       ) =
    [BT12.concat $ reverse base]
combineOutput (CombineState base _        Nothing ) =
    [BT12.concat $ reverse base]
combineOutput (CombineState base (Just c) (Just t)) =
    [BT12.concat . reverse $ (t:c:base)]

