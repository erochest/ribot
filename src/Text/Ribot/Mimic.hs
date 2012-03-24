
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

import qualified Data.Text as T
import           Database.Ribot

mimic :: [Message] -> IO [T.Text]
mimic msgs = return []

