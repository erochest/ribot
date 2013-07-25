
-- This handles the `!topic` command, which searches the database and prints
-- out the results or posts them to PasteBin.

module Network.Ribot.Irc.Part.Topic
    ( topicPart
    , topicCommand
    ) where

import           Control.Monad.IO.Class (liftIO)
import qualified Database.Ribot.Search as S
import           Network.IRC.Bot.BotMonad (BotMonad(..), maybeZero)
import           Network.IRC.Bot.Commands (askSenderNickName, replyTo)
import           Network.IRC.Bot.Log (LogLevel(Debug))
import           Network.IRC.Bot.Parsec (botPrefix, parsecPart)
import           Network.Ribot.PasteBin (PasteBinApiKey, bulkPrivMsg)
import           Text.Parsec (ParsecT, (<|>), anyChar, many, space
                             , string, try, option)

-- This is the part to integrate into Ribot.
topicPart :: BotMonad m => FilePath -> Maybe PasteBinApiKey -> Int -> m ()
topicPart dbFile pbKey searchMax =
    parsecPart (topicCommand dbFile pbKey searchMax)

-- This parses and handles the `!search` command.
topicCommand :: BotMonad m
             => FilePath
             -> Maybe PasteBinApiKey
             -> Int
             -> ParsecT String () m ()
topicCommand dbFile pbKey searchMax = topic <|> return ()
    where
        topic :: BotMonad m => ParsecT String () m ()
        topic = try $ do
            botPrefix
            _       <- string "topic"
            terms   <- option "" (space >> many anyChar)
            target  <- maybeZero =<< replyTo
            nick    <- maybeZero =<< askSenderNickName
            logM Debug . ("!topic " ++) $ show terms
            replies <- liftIO $ S.topic dbFile searchMax terms
            logM Debug . ("!topic => " ++) $ show (length replies)
            let rc     = length replies
                header = nick ++ ", " ++ show rc ++ plural rc " result"
            bulkPrivMsg pbKey target header $ map show replies

        plural :: Int -> String -> String
        plural 1 s = s
        plural _ s = s ++ "s"


