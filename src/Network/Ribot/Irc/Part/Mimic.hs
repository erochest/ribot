{-# LANGUAGE OverloadedStrings #-}

-- This handles the `!mimic` command, which creates a simple MM of a nick's
-- messages and generates a random message that mimics that user.

module Network.Ribot.Irc.Part.Mimic
    ( mimicPart
    , mimicCommand
    ) where

import           Network.IRC.Bot.BotMonad (BotMonad(..))
import           Network.IRC.Bot.Parsec (parsecPart)
import           Text.Parsec (ParsecT)
import           Network.Ribot.Irc.Utils
import qualified Text.Ribot.Mimic as M

mimicPart :: BotMonad m => FilePath -> m ()
mimicPart dbFile = parsecPart (mimicCommand dbFile)

mimicCommand :: BotMonad m => FilePath -> ParsecT String () m ()
mimicCommand = messageCommand "mimic" M.mimic

