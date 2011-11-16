
-- This module keeps track of the probabilities of a word following two other
-- words. This can be used to generate a third item from a sequence of two
-- items, and by extension, a string of many items.

module Text.Ribot.Generate
    ( TextGenerator
    , mkTextGenerator
    , mostLikely
    , randomContinuation
    , chain
    , triples
    ) where

import qualified Data.List as L
import qualified Data.Map as M
import           Data.Maybe
import qualified Data.Ord as O
import           System.Random

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
        combine hmm (obs1, obs2, next) =
            M.alter (combine' next) (obs1, obs2) hmm

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
getContinuationList (TextGenerator hmm) seq =
    return . L.sortBy (O.comparing key) . M.toList =<< M.lookup seq hmm
    where
        key :: (a, Int) -> Int
        key (_, i) = -i

-- This returns the most likely continuation of the sequence given. If there's
-- no data for that sequence, `Nothing` is returned.
mostLikely :: Ord a => TextGenerator a -> (a, a) -> Maybe a
mostLikely textGen seq =
    fmap fst . listToMaybe =<< getContinuationList textGen seq

-- This returns a continuation for the sequence. It does using a random
-- selecting weighted by the frequency of each continuation. If there aren't
-- any continuations, `Nothing` is returned.
randomContinuation :: Ord a => TextGenerator a -> (a, a) -> IO (Maybe a)
randomContinuation textGen seq =
    case (getContinuationList textGen seq) of
        Nothing   -> return Nothing
        Just cont -> randomIO >>= return . getWeightedChoice cont

-- This takes a list of items with weights and a percent as a fraction and
-- returns the item with that weighted amount. It walks through the list and
-- takes the item for which the running weight total takes it over the weight
-- fraction passed in.
getWeightedChoice :: [(a, Int)] -> Double -> Maybe a
getWeightedChoice choices cutOff = getChoice cutOff total 0.0 choices
    where
        total = L.sum $ map snd choices

-- This uses `foldl` to decorate every item from the input with its running
-- total weight as a percent of the total. `foldl` can thread the state
-- through, but it complicates lazy evaluation.
getChoice :: Double -> Int -> Double -> [(a, Int)] -> Maybe a
getChoice _ _ _ [] = Nothing
getChoice cutOff total run ((item, weight):_) | (run + (fromIntegral weight) / fromIntegral total) >= cutOff =
    Just item
getChoice cutOff total run ((_, weight):items) =
    getChoice cutOff total (run + (fromIntegral weight) / fromIntegral total) items

-- This creates an infinite chain of items. You can limit it using `take` or
-- `takeWhile` or something. The item given is the item to use for placeholders
-- at the beginning and end of sequences.
chain :: TextGenerator a -> a -> [a]
chain _ _ = []

-- This breaks a list of items into a list of overlapping triples. The first
-- position is for a boundary marker, so this call,
--
--     triples -1 [1..5]
--     [(-1, -1, 1), (-1, 1, 2), (1, 2, 3), (2, 3, 4), (3, 4, 5), (4, 5, -1),
--      (5, -1, -1)]
triples :: a -> [a] -> [(a, a, a)]
triples _ _ = []

