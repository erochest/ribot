
-- This complains whenever anyone uses '@NICK'.

module Network.Ribot.Irc.Part.Knitter
    ( knitterPart
    , knitterCommand
    ) where

import           Control.Applicative ((<$>))
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class (liftIO, MonadIO)
import           Data.Maybe
import qualified Data.Text as T
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Ribot
import           Network.IRC.Bot.BotMonad (BotMonad(..), maybeZero, BotEnv,
                                           runBotPartT)
import           Network.IRC.Bot.Commands (PrivMsg(..), askSenderNickName,
                                           replyTo, sendCommand)
import           Network.IRC.Bot.Log (LogLevel(Debug))
import           Network.IRC.Bot.Parsec (parsecPart)
import           System.Random
import           Text.Parsec (ParsecT, (<|>), many1, try, skipMany, noneOf
                             , char, alphaNum)

knitterPart :: BotMonad m => FilePath -> m ()
knitterPart dbFile = parsecPart (knitterCommand dbFile)

knitterCommand :: BotMonad m => FilePath -> ParsecT String () m ()
knitterCommand dbFile = knitter <|> return ()
    where
        knitter :: BotMonad m => ParsecT String () m ()
        knitter = try $ do
            skipMany $ noneOf "@"
            char '@'
            name   <- many1 (alphaNum <|> char '-')

            exists <- liftIO . isUser $ T.pack name

            when exists $ do
                target <- maybeZero =<< replyTo
                nick   <- maybeZero =<< askSenderNickName

                logM Debug $ nick ++ " thinks they're on twitter."
                sendCommand $ PrivMsg Nothing [target] $ nick ++ ", THIS ISN'T TWITTER!"

                env <- askBotEnv
                liftIO . forkIO $ mutter env target

                return ()

        isUser :: T.Text -> IO Bool
        isUser userName =
            withSqliteConn (T.pack dbFile) $ runSqlConn $ isUser' userName

        isUser' userName = isJust <$> getBy (UniqueUser userName)

        mutter :: BotEnv -> String -> IO ()
        mutter env target = do
            delay <- randomRIO (1000000, 10000000)
            threadDelay delay

            n <- randomRIO (0, length mutterings - 1)
            runBotPartT (sendCommand . PrivMsg Nothing [target] $ mutterings !! n)
                        env

        mutterings :: [String]
        mutterings =
            [ "dammit"
            , "GET OFF MY LAWN!"
            , "twitter-smitter"
            , "kids these days"
            , "sheesh"
            ]

