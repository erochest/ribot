{-# LANGUAGE OverloadedStrings #-}

-- This is the entry point for Ribot.

module Main where

import           Control.Concurrent
import qualified Data.Configurator as C
import           Data.Configurator.Types (Config)
import           Data.Ribot.Config
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
            config'    <- createBotConf configFile writeMsg

            case config' of
                Nothing  -> putStrLn "You must are missing configuration keys."
                Just cfg -> runBot configFile cfg

runBot :: Config -> BotConf -> IO ()
runBot cfg botConfig = do
    asDaemon <- C.lookupDefault False cfg "daemonize"
    if asDaemon
        then daemonize $ runDaemon botConfig
        else runConsole botConfig

writeMsg :: Chan Message -> IO ()
writeMsg chan = readChan chan >>= putStrLn . ("MESSAGE: " ++) . show

