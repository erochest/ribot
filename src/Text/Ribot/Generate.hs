
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

-- This is the data type for holding the information about the statistical
-- model. Currently, this is just a stub.
data TextGenerator a = TextGenerator ()

-- This is the constructor for `TextGenerator` data.
mkTextGenerator :: [(a, a, a)] -> TextGenerator a
mkTextGenerator _ = TextGenerator ()

-- This returns the most likely continuation of the sequence given. If there's
-- no data for that sequence, `Nothing` is returned.
mostLikely :: TextGenerator a -> (a, a) -> Maybe a
mostLikely _ _ = Nothing

-- This returns a continuation for the sequence. It does using a random
-- selecting weighted by the frequency of each continuation. If there aren't
-- any continuations, `Nothing` is returned.
randomContinuation :: TextGenerator a -> (a, a) -> Maybe a
randomContinuation _ _ = Nothing

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

