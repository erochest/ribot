{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

-- This is the entry point for Ribot.

module Main where

import           Control.Concurrent
import           Control.Monad (forM_)
import           Control.Monad.IO.Class
import           Data.Configurator.Types (Config)
import           Data.Ribot.Config
import qualified Data.Text as T
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Ribot hiding (Message)
import qualified Database.Ribot as D
import           Database.Ribot.Index (indexItem, reindex)
import           Database.Ribot.Search (search)
import           Network.IRC.Base
import           Network.IRC.Bot
import           Network.Ribot.Irc
import           Ribot.Cli
import           System.Console.CmdArgs (cmdArgs)
import           System.Posix.Daemonize (daemonize)
import qualified Text.Ribot.Mimic as M


main :: IO ()
main = do
    mode <- cmdArgs ribotModes
    case config mode of
        Just fileName -> do
            ircConfig <- readConfig fileName
            dbFile    <- ribotDbFile ircConfig
            handleMode mode ircConfig dbFile
        Nothing       -> putStrLn "You must specify a configuration file."

-- This handles dispatching to the modes.
handleMode :: Modes -> Config -> String -> IO ()
handleMode (Listen _) ircConfig dbFile = do
    config' <- createBotConf ircConfig $ writeMsg dbFile
    case config' of
        Just cfg -> runBot ircConfig cfg
        Nothing  -> putStrLn "You are missing configuration keys."
handleMode (Search _ searchTerms) ircConfig dbFile = do
    searchMax <- ribotSearchMax ircConfig
    replies   <- search dbFile searchMax terms'
    putStrLn $ "SEARCH: " ++ terms'
    putStrLn $ show (length replies) ++ " results."
    forM_ replies print
    where terms' = unwords searchTerms
handleMode (Ribot.Cli.Topic _ _) _ _ =
    return ()
handleMode (Reindex _) _ dbFile = do
    runPool dbFile 4 reindex
    putStrLn "done"
handleMode (Mimic _ userName) _ dbFile =
    runDb dbFile $ do
        msgs'  <- getUserMessages userName'
        output <- case msgs' of
            Nothing -> return $ T.concat [ "I haven't heard "
                                         , userName'
                                         , " say anthying."
                                         ]
            Just [] -> return $ T.concat [ userName'
                                         , " hasn't said anything."
                                         ]
            Just msgs -> do
                let messages = map messageObj msgs
                liftIO $ T.unwords `fmap` M.mimic messages
        liftIO . putStrLn $ T.unpack output
    where
        userName' = T.pack userName
        messageObj :: Entity D.Message -> D.Message
        messageObj (Entity _ m) = m

-- This initializes the database and runs the bot either as a daemon or a
-- console program.
runBot :: Config -> BotConf -> IO ()
runBot cfg botConfig = do
    dbFile <- ribotDbFile cfg
    initDatabase dbFile
    asDaemon <- ribotDaemonize cfg
    if asDaemon
        then daemonize $ runDaemon botConfig cfg
        else runConsole botConfig cfg

-- This processes the messages by logging and indexing them.
writeMsg :: FilePath -> Chan Message -> IO ()
writeMsg dbFile chan = runPool dbFile 4 $
    liftIO $ getChanContents chan >>= mapM_ process
    where
        process msg' = withSqliteConn (T.pack dbFile) $ runSqlConn $
            saveMessage msg' >>= indexItem

