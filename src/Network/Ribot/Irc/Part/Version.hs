
-- This handles the `!version` command.

module Network.Ribot.Irc.Part.Version
    ( versionPart
    , versionCommand
    ) where

import           Data.Version (showVersion)
import           Network.IRC.Bot.BotMonad (BotMonad(..), maybeZero)
import           Network.IRC.Bot.Commands (PrivMsg(..), askSenderNickName,
                                           replyTo, sendCommand)
import           Network.IRC.Bot.Log (LogLevel(Debug))
import           Network.IRC.Bot.Parsec (botPrefix, parsecPart)
import           Paths_ribot (version)
import           Text.Parsec (ParsecT, (<|>), anyChar, many1, optionMaybe,
                              space, string, try)

-- This integrates this part into the bot.
versionPart :: BotMonad m => m ()
versionPart = parsecPart versionCommand

-- This handles parsing the command and responding.
versionCommand :: BotMonad m => ParsecT String () m ()
versionCommand = versionCommand' <|> return ()
    where
        versionCommand' :: BotMonad m => ParsecT String () m ()
        versionCommand' = try $ do
            botPrefix
            string "version"
            target <- maybeZero =<< replyTo
            nick   <- maybeZero =<< askSenderNickName
            let version' = showVersion version
            logM Debug $ "!version => " ++ version'
            sendCommand . PrivMsg Nothing [target] $ nick ++ ", v" ++ version'

