{-# LANGUAGE OverloadedStrings #-}

-- | This handles the `!bingo` command, which returns a list of 25 words,
-- randomly weighted.

module Network.Ribot.Irc.Part.Bingo
    ( bingoPart
    , bingoCommand
    ) where

import           Network.IRC.Bot.BotMonad (BotMonad(..))
import           Network.IRC.Bot.Parsec (parsecPart)
import           Network.Ribot.Irc.Utils
import           Text.Parsec (ParsecT)
import qualified Text.Ribot.Bingo as B

bingoPart :: BotMonad m => FilePath -> m ()
bingoPart = parsecPart . bingoCommand

bingoCommand :: BotMonad m => FilePath -> ParsecT String () m ()
bingoCommand = messageCommand "bingo" B.bingo

