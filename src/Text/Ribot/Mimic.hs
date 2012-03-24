{-# LANGUAGE OverloadedStrings #-}

-- This modules keeps track of the probabilities of a word following two other
-- words. This can be used to generate a third item from a sequence of two
-- items, and by extension, a string of many items.

module Text.Ribot.Mimic
    ( mimic
    {-
     - ( TextGenerator
     - , mkTextGenerator
     - , mostLikely
     - , randomContinuation
     - , chain
     - , triples
     - , mimic
     -}
    ) where

import           Control.Monad (liftM)
import qualified Data.Either as E
import qualified Data.List as L
import qualified Data.Map as M
import           Data.Maybe
import qualified Data.Ord as O
import qualified Data.Set as S
import qualified Data.Text as T
import           Database.Ribot
import           System.Random
-- [Text.Ribot.Tokenizer](Tokenizer.html)
import           Text.Ribot.Tokenizer
import qualified Text.Bakers12.Tokenizer.Types as BT12


-- This takes a list of message texts and a number of items to generate. It
-- tokenizes the messages, breaks them into triples, and creates a
-- `TextGenerator` from that data. It uses that generator to assemble a
-- mimicking utterance for the person.
mimic :: [Message] -> IO [T.Text]
mimic []   = return []
mimic msgs = chain tg "#" 12
    where
        tokens = map (map BT12.tokenText)
               . E.rights
               . map (tokenize "" . messageText)
               $ msgs
        trips  = L.concatMap (triples "#") tokens
        tg     = mkTextGenerator trips


-- This is a frequency map between an item and its frequency.
type FreqMap a = M.Map a Int

-- This is a mapping between two observations and a `FreqMap` of the items that
-- immediately follow it.
type Model a = M.Map (a, a) (FreqMap a)

-- This is the data type for holding the information about the statistical
-- model. Currently, this is just a stub.
data TextGenerator a = TextGenerator (Model a)

-- This is the constructor for `TextGenerator` data. It takes a set of data to
-- train on and returns the constructed generator.
mkTextGenerator :: Ord a => [(a, a, a)] -> TextGenerator a
mkTextGenerator = TextGenerator . L.foldl' combine M.empty
    where
        -- This handles one step of the fold.
        combine :: Ord a => Model a -> (a, a, a) -> Model a
        combine hmm (obs1, obs2, nextStep) =
            M.alter (combine' nextStep) (obs1, obs2) hmm

        -- This increments a sequence's `FreqMap`.
        combine' :: Ord a => a -> Maybe (FreqMap a) -> Maybe (FreqMap a)
        combine' item Nothing   = Just $ M.singleton item 1
        combine' item (Just fm) = Just $ M.alter incr item fm

        -- This increments an item's count in a `FreqMap`.
        incr :: Maybe Int -> Maybe Int
        incr Nothing  = Just 1
        incr (Just n) = Just (n + 1)

-- This returns the continuations for a leading sequence in descending order of
-- frequency.
getContinuationList :: Ord a => TextGenerator a -> (a, a) -> Maybe [(a, Int)]
getContinuationList (TextGenerator hmm) leadingSeq =
    return . L.sortBy (O.comparing key) . M.toList =<< M.lookup leadingSeq hmm
    where
        key :: (a, Int) -> Int
        key (_, i) = -i

-- This returns the most likely continuation of the sequence given. If there's
-- no data for that sequence, `Nothing` is returned.
mostLikely :: Ord a => TextGenerator a -> (a, a) -> Maybe a
mostLikely textGen leadingSeq =
    fmap fst . listToMaybe =<< getContinuationList textGen leadingSeq

-- This returns a continuation for the sequence. It does using a random
-- selecting weighted by the frequency of each continuation. If there aren't
-- any continuations, `Nothing` is returned.
randomContinuation :: Ord a => TextGenerator a -> (a, a) -> IO (Maybe a)
randomContinuation textGen leadingSeq =
    case getContinuationList textGen leadingSeq of
        Nothing   -> return Nothing
        Just cont -> liftM (getWeightedChoice cont) randomIO

-- This takes a list of items with weights and a percent as a fraction and
-- returns the item with that weighted amount. It walks through the list and
-- takes the item for which the running weight total takes it over the weight
-- fraction passed in.
getWeightedChoice :: [(a, Int)] -> Double -> Maybe a
getWeightedChoice choices cutOff =
    -- This is a mouthful, and you have to read it backwards. Here's what it
    -- does:
    --
    -- * `L.mapAccumL accum 0 choices` — Convert the weights in the list of
    -- choices into a percentage of the total weights in the list;
    -- * `snd` — Drop off the accumulator's state, the running total;
    -- * `L.dropWhile ((< cutOff) . snd)` — Remove all of the items from the
    -- front of the list until we get to the item whose running weight total
    -- percentage is at or above the cut off (i.e., remove all the items up to
    -- the one we want);
    -- * `map fst` — Remove the running weighted total percentage and save only
    -- the item;
    -- * `listToMaybe` — If there is anything in the list, take the head and
    -- wrap it in `Just`; otherwise, return `Nothing`.
    listToMaybe
        . map fst
        . L.dropWhile ((< cutOff) . snd)
        . snd
        $ L.mapAccumL accum 0 choices

    where
        total = fromIntegral . L.sum $ map snd choices

        -- This handles accumulating the running total information. It takes
        -- each pair from the choice list and replaces the weight with the
        -- running percentage of the weight totals.
        accum :: Int -> (a, Int) -> (Int, (a, Double))
        accum running (item, weight) =
            (running', (item, fromIntegral running' / total))
            where running' = running + weight

-- This creates a chain of items. The item given is the item to use for
-- placeholders at the beginning and end of sequences. The number is the number
-- of tokens to output in the sequence.
--
-- First, this will get all the sequences in the generator that start with the
-- initial character. It then picks one at random. From there, it keeps calling
-- `randomConintuation` until it doesn't need more items.
--
-- This is kind of messy. I'm not very happy with it.
chain :: Ord a => Show a => TextGenerator a -> a -> Int -> IO [a]
chain textGen@(TextGenerator tg) start n = do
    (_, nextItem) <- randomElem nextPairs
    return . filter (/= start) =<< chain' textGen start start nextItem n
    where
        -- This is a list of all the sequences in the model that begin with
        -- `start`.
        nextPairs = L.filter ((start ==) . fst) $ M.keys tg

        -- This is a set of all the options. It's used to randomly select a
        -- continuation for where there are holes in the model.
        itemSet :: Ord b => Model b -> S.Set b
        itemSet m = S.fromList . L.concatMap (uncurry getItems) $ M.toList m
            where
                getItems :: Ord c => (c, c) -> FreqMap c -> [c]
                getItems (t1, t2) freqMap = t1 : t2 : M.keys freqMap

        -- Just `itemSet` as a list so we can easily pick one at random.
        itemList :: Ord d => Model d -> [d]
        itemList = S.toList . itemSet

        randomElem :: Ord a => [a] -> IO a
        randomElem list =
            liftM (list !!) (randomRIO (0, length list - 1))

        -- This constructs a list/stream that pulls.
        chain' :: Ord e => Show e => Eq e =>
                  TextGenerator e -> e -> e -> e -> Int -> IO [e]
        chain' _ _ _ _ 0 = return []
        chain' tg'@(TextGenerator m) filterOut token1 token2 thisN = do
            contMaybe <- randomContinuation tg' (token1, token2)
            case contMaybe of
                Just token3 -> do
                    rest <- chain' tg' filterOut token2 token3 n'
                    return (token1 : rest)
                Nothing     -> do
                    token3 <- randomElem (itemList m)
                    rest   <- chain' tg' filterOut token2 token3 n'
                    return (token1 : rest)
            where n' = if token1 == filterOut
                        then thisN
                        else thisN - 1

-- This breaks a list of items into a list of overlapping triples. The first
-- position is for a boundary marker, so this call,
--
--     triples -1 [1..5]
--     [(-1, -1, 1), (-1, 1, 2), (1, 2, 3), (2, 3, 4), (3, 4, 5), (4, 5, -1),
--      (5, -1, -1)]
triples :: a -> [a] -> [(a, a, a)]
triples fill xs = L.zip3 xxs (L.drop 1 xxs) (L.drop 2 xxs)
    where xxs = (fill : fill : xs) ++ [fill, fill]


