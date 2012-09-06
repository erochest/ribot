
module Text.Ribot.Bingo
    ( bingo
    ) where

import           Control.Applicative
import           Control.Monad (replicateM)
import qualified Data.Either as E
import qualified Data.List as L
import qualified Data.Map as M
import           Data.Maybe
import qualified Data.Text as T
import           Database.Ribot
import           System.Random
import           Text.Ribot.Mimic (FreqMap, getWeightedChoice)
import           Text.Ribot.Tokenizer
import qualified Text.Bakers12.Tokenizer.Types as BT12

bingo :: [Message] -> IO [T.Text]
bingo []   = return []
bingo msgs = randomSelect tokens 25
    where
        tokens = concatMap (map BT12.tokenText)
               . E.rights
               . map (tokenize "" . messageText)
               $ msgs

randomSelect :: [T.Text] -> Int -> IO [T.Text]
randomSelect tokens n =
    catMaybes <$> replicateM n (getWeightedChoice fs <$> randomIO)
    where
        fs = M.toList $ freqs tokens

freqs :: [T.Text] -> FreqMap T.Text
freqs = L.foldl' incr M.empty
    where incr m k = M.insertWith (+) k 1 m

