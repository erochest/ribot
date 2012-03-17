
-- This handles the `!search` command, which searches the database and prints
-- out the results or posts them to PasteBin.

module Network.Ribot.Irc.Part.Search
    ( searchPart
    , searchCommand
    ) where

import           Control.Monad (forM_)
import           Control.Monad.IO.Class (liftIO)
import qualified Database.Ribot.Search as S
import           Network.IRC.Bot.BotMonad (BotMonad(..), maybeZero)
import           Network.IRC.Bot.Commands (PrivMsg(..), askSenderNickName,
                                           replyTo, sendCommand)
import           Network.IRC.Bot.Log (LogLevel(Debug))
import           Network.IRC.Bot.Parsec (botPrefix, parsecPart)
import           Text.Parsec (ParsecT, (<|>), anyChar, many, many1, optionMaybe,
                              space, string, try)

-- This is the part to integrate into Ribot.
searchPart :: BotMonad m => FilePath -> Int -> m ()
searchPart dbFile searchMax = parsecPart (searchCommand dbFile searchMax)

-- This parses and handles the `!search` command.
searchCommand :: BotMonad m => FilePath -> Int -> ParsecT String () m ()
searchCommand dbFile searchMax = search <|> return ()
    where
        search :: BotMonad m => ParsecT String () m ()
        search = try $ do
            botPrefix
            string "search"
            terms   <- space >> many anyChar
            target  <- maybeZero =<< replyTo
            nick    <- maybeZero =<< askSenderNickName
            logM Debug . ("!search " ++) $ show terms
            replies <- liftIO $ S.search dbFile searchMax terms
            logM Debug . ("!search => " ++) $ show (length replies)
            -- PasteBin
            forM_ replies $ \r ->
                sendCommand . PrivMsg Nothing [target] $ nick ++ ", " ++ show r

