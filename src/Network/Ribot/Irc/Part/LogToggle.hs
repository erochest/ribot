
-- This handles the `!log` command, which turns logging on and off for the user
-- issuing it.

module Network.Ribot.Irc.Part.LogToggle
    ( logTogglePart
    , logToggleCommand
    ) where

import           Control.Monad.IO.Class (liftIO, MonadIO)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Resource
import qualified Data.Text as T
import           Database.Ribot
import           Database.Persist (Entity(..))
import           Network.IRC.Bot.BotMonad (BotMonad(..), maybeZero)
import           Network.IRC.Bot.Commands (PrivMsg(..), askSenderNickName,
                                           replyTo, sendCommand)
import           Network.IRC.Bot.Log (LogLevel(Debug))
import           Network.IRC.Bot.Parsec (botPrefix, parsecPart)
import           Text.Parsec (ParsecT, (<|>), anyChar, many1, optionMaybe,
                              space, string, try)

-- This is the part to integrate into Ribot.
logTogglePart :: BotMonad m => FilePath -> m ()
logTogglePart dbFile = parsecPart (logToggleCommand dbFile)

-- This parses and handles the `!log` command.
logToggleCommand :: BotMonad m => FilePath -> ParsecT String () m ()
logToggleCommand dbFile = log <|> return ()
    where
        log :: BotMonad m => ParsecT String () m ()
        log = try $ do
            botPrefix
            string "log"
            setting <- optionMaybe $ space >> bool
            target  <- maybeZero =<< replyTo
            nick    <- maybeZero =<< askSenderNickName
            reply   <- liftIO $ saveLogging dbFile (T.pack nick) setting
            logM Debug . ("!log " ++) $ show setting
            sendCommand . PrivMsg Nothing [target] $ nick ++ ", " ++ reply

saveLogging :: FilePath -> T.Text -> Maybe Bool -> IO String
saveLogging dbFile userName (Just logging) = runDb dbFile $ do
    (Entity userId user) <- getOrCreateUser userName
    setUserLogging userId logging
    return $ logMessage logging
saveLogging dbFile userName Nothing = runDb dbFile $ do
    (Entity userId user) <- getOrCreateUser userName
    return . logMessage $ userLoggingOn user

logMessage :: Bool -> String
logMessage True  = "logging is turned on."
logMessage False = "logging is turned off."

bool :: BotMonad m => ParsecT String () m Bool
bool = do
        (try (string "true")  >> return True)
    <|> (try (string "yes")   >> return True)
    <|> (try (string "on")    >> return True)
    <|> (try (string "t")     >> return True)
    <|> (try (string "y")     >> return True)
    <|> (try (string "false") >> return False)
    <|> (try (string "no")    >> return False)
    <|> (try (string "off")   >> return False)
    <|> (try (string "f")     >> return False)
    <|> (try (string "n")     >> return False)


