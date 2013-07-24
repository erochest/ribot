{-# LANGUAGE OverloadedStrings #-}

module Network.Ribot.Irc.Utils
    ( messageCommand
    ) where

import           Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Ribot (getUserMessages, Message, withResourceLogger)
import           Network.IRC.Bot.BotMonad (BotMonad(..), maybeZero)
import           Network.IRC.Bot.Commands (PrivMsg(..), replyTo, sendCommand)
import           Network.IRC.Bot.Log (LogLevel(Debug))
import           Network.IRC.Bot.Parsec (botPrefix)
import           Text.Parsec (ParsecT, (<|>), anyChar, many, space, string, try)

-- | This takes a command that takes a user name, a function that converts
-- a list of messages into a list of output strings, and the database file
-- name, and it handles that message.
messageCommand :: BotMonad m
               => String
               -- ^ The command name.
               -> ([Message] -> IO [T.Text])
               -- ^ The function that takes the message and generates the
               -- output.
               -> FilePath
               -- ^ The database file.
               -> ParsecT String () m ()
messageCommand name f dbFile = command <|> return ()
    where
        command :: BotMonad m => ParsecT String () m ()
        command = try $ do
            botPrefix
            string name

            user   <- space >> many anyChar
            target <- maybeZero =<< replyTo

            output <- liftIO $ T.unpack `fmap` getOutput (T.pack user)
            logM Debug $ "!" ++ name ++ " " ++ user ++ " => " ++ output
            sendCommand $ PrivMsg Nothing [target] output

        getOutput :: T.Text -> IO T.Text
        getOutput userName = do
            msgs' <- getUserMessages' userName
            case msgs' of
                Nothing -> return $ T.concat [ "I haven't heard "
                                             , userName
                                             , " say anything."
                                             ]
                Just [] -> return $ T.concat [ userName
                                             , " hasn't said anything."
                                             ]
                Just msgs -> do
                    let messages = map entityVal msgs
                    liftIO $ T.unwords `fmap` f messages

        getUserMessages' :: T.Text -> IO (Maybe [Entity Message])
        getUserMessages' userName =
            withResourceLogger . withSqliteConn (T.pack dbFile) $ runSqlConn $ getUserMessages userName

