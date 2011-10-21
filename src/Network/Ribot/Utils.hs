
-- This is a collection of utility functions.

module Network.Ribot.Utils
    ( split
    ) where

import qualified Data.List as L


-- This splits a list (string) on a character.
split :: Eq a => a -> [a] -> [[a]]
split _  []   = []
split on list = case rest of
                    (on:rest') -> prefix : split on rest'
                    []         -> [prefix]
    where (prefix, rest) = span (/=on) list

