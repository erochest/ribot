{-# LANGUAGE OverloadedStrings #-}

-- This is the entry point for Ribot.

module Main where

import           Control.Concurrent
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Configurator as C
import           Data.Configurator.Types (Config)
import           Data.Ribot.Config
import           Database.Ribot
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
    ribotDbFile cfg >>= initDatabase
    asDaemon <- ribotDaemonize cfg
    if asDaemon
        then daemonize $ runDaemon botConfig
        else runConsole botConfig

writeMsg :: FilePath -> Chan Message -> IO ()
writeMsg dbFile chan = runDb dbFile $
    (liftIO $ getChanContents chan) >>= mapM_ saveMessage

