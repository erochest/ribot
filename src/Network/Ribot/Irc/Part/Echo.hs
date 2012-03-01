
-- This implements a simple `!echo` command that repeats back whatever follows
-- the command.

module Network.Ribot.Irc.Part.Echo
    ( echoPart
    , echoCommand
    ) where

import           Network.IRC.Bot.BotMonad (BotMonad(..), maybeZero)
import           Network.IRC.Bot.Commands (PrivMsg(..), replyTo, sendCommand)
import           Network.IRC.Bot.Log (LogLevel(Debug))
import           Network.IRC.Bot.Parsec (botPrefix, parsecPart)
import           Text.Parsec (ParsecT, (<|>), anyChar, many1, space, string, try)

-- This is the part to integrate into Ribot.
echoPart :: BotMonad m => m ()
echoPart = parsecPart echoCommand

-- This parses and handles the `!echo` command.
echoCommand :: BotMonad m => ParsecT String () m ()
echoCommand = echo' <|> return ()
    where 
        echo' = do
            input <- try $ do
                botPrefix
                string "echo"
                space
                many1 anyChar
            target <- maybeZero =<< replyTo
            logM Debug $ "echo " ++ show target
            sendCommand $ PrivMsg Nothing [target] input

