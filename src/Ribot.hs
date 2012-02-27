{-# LANGUAGE OverloadedStrings #-}

-- This is the entry point for Ribot.

module Main where

import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.Chan
import           Control.Monad (forever, when)
import qualified Data.Configurator as C
import           Data.Configurator.Types (Config)
import           Data.Ribot.Config
import qualified Data.Set as S
import qualified Data.Text as T
import           Data.Version (showVersion)
import           Network
import           Network.BSD
import           Network.IRC.Base
import           Network.IRC.Bot
import           Network.Ribot.Irc
import           Paths_ribot (version)
import           System.Console.CmdArgs
import           System.Posix.Daemonize (daemonize)


-- Modes for the CLI.
data Modes
    = Listen
        { config :: Maybe FilePath
        }
    deriving (Data, Show, Typeable)

ribotModes :: Modes
ribotModes = modes
    [ Listen
        { config = def &= name "c" &= help "The location of the configuration file."
        } &= details ["This listens on an IRC channel."] &= auto
    ] &= summary ("ribot v" ++ showVersion version)
      &= program "ribot"


main :: IO ()
main = do
    mode <- cmdArgs ribotModes
    case config mode of
        Nothing       -> putStrLn "You must specify a configuration file."
        Just fileName -> do
            configFile <- readConfig fileName
            config'    <- createBotConf configFile writeMsg

            case config' of
                Nothing     -> putStrLn "You must are missing configuration keys."
                Just config -> runBot configFile config

runBot :: Config -> BotConf -> IO ()
runBot config botConfig = do
    asDaemon <- C.lookupDefault False config "daemonize"
    if asDaemon
        then daemonize $ runDaemon botConfig
        else runConsole botConfig

writeMsg :: Chan Message -> IO ()
writeMsg chan = readChan chan >>= putStrLn . ("MESSAGE: " ++) . show

