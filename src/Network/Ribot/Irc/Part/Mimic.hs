{-# LANGUAGE OverloadedStrings #-}

-- This handles the `!mimic` command, which creates a simple MM of a nick's
-- messages and generates a random message that mimics that user.

module Network.Ribot.Irc.Part.Mimic
    ( mimicPart
    , mimicCommand
    ) where

import           Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Ribot (getUserMessages, Message)
import           Network.IRC.Bot.BotMonad (BotMonad(..), maybeZero)
import           Network.IRC.Bot.Commands (PrivMsg(..), replyTo, sendCommand)
import           Network.IRC.Bot.Log (LogLevel(Debug))
import           Network.IRC.Bot.Parsec (botPrefix, parsecPart)
import           Text.Parsec (ParsecT, (<|>), anyChar, many, space, string, try)
import qualified Text.Ribot.Mimic as M

mimicPart :: BotMonad m => FilePath -> m ()
mimicPart dbFile = parsecPart (mimicCommand dbFile)

mimicCommand :: BotMonad m => FilePath -> ParsecT String () m ()
mimicCommand dbFile = mimic <|> return ()
    where
        mimic :: BotMonad m => ParsecT String () m ()
        mimic = try $ do
            botPrefix
            string "mimic"
            user     <- space >> many anyChar

            target   <- maybeZero =<< replyTo

            output <- liftIO $ T.unpack `fmap` getMimic (T.pack dbFile) (T.pack user)
            logM Debug $ "!mimic " ++ user ++ " => " ++ output
            sendCommand $ PrivMsg Nothing [target] output

        messageObj :: Entity Message -> Message
        messageObj (Entity _ m) = m

        getMimic :: T.Text -> T.Text -> IO T.Text
        getMimic dbFile' userName = do
            msgs'  <- getUserMessages' dbFile' userName
            case msgs' of
                Nothing -> return $ T.concat [ "I haven't heard "
                                             , userName
                                             , " say anthying."
                                             ]
                Just [] -> return $ T.concat [ userName
                                             , " hasn't said anything."
                                             ]
                Just msgs -> do
                    let messages = map messageObj msgs
                    liftIO $ T.unwords `fmap` M.mimic messages

        getUserMessages' :: T.Text -> T.Text -> IO (Maybe [Entity Message])
        getUserMessages' dbFile' userName =
            withSqliteConn dbFile' $ runSqlConn $ getUserMessages userName

