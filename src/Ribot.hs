{-# LANGUAGE OverloadedStrings #-}

-- This is the entry point for Ribot.

module Main where

import           Control.Concurrent
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Resource (ResourceIO)
import qualified Data.Configurator as C
import           Data.Configurator.Types (Config)
import           Data.Ribot.Config
import           Database.Persist.Sqlite
import           Database.Ribot hiding (Message)
import           Database.Ribot.Index (indexItem)
import           Network.IRC.Base
import           Network.IRC.Bot
import           Network.Ribot.Irc
import           Ribot.Cli
import           System.Console.CmdArgs (cmdArgs)
import           System.Posix.Daemonize (daemonize)


main :: IO ()
main = do
    mode <- cmdArgs ribotModes
    case config mode of
        Nothing       -> putStrLn "You must specify a configuration file."
        Just fileName -> do
            configFile <- readConfig fileName
            dbFile     <- ribotDbFile configFile
            config'    <- createBotConf configFile $ writeMsg dbFile

            case config' of
                Nothing  -> putStrLn "You must are missing configuration keys."
                Just cfg -> runBot configFile cfg

runBot :: Config -> BotConf -> IO ()
runBot cfg botConfig = do
    dbFile <- ribotDbFile cfg
    initDatabase dbFile
    asDaemon <- ribotDaemonize cfg
    if asDaemon
        then daemonize $ runDaemon botConfig dbFile
        else runConsole botConfig dbFile

writeMsg :: FilePath -> Chan Message -> IO ()
writeMsg dbFile chan = runPool dbFile 4 $
    (liftIO $ getChanContents chan) >>= mapM_ process
    where
        process :: ResourceIO m => Message -> SqlPersist m ()
        process msg = saveMessage msg >>= indexItem

