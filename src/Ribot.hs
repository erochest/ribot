{-# LANGUAGE OverloadedStrings #-}

-- This is the entry point for Ribot.

module Main where

import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.Chan
import           Control.Monad (forever, when)
import qualified Data.Configurator as C
import           Data.Configurator.Types (Config)
import qualified Data.Set as S
import qualified Data.Text as T
import           Data.Version (showVersion)
import           Network
import           Network.BSD
import           Network.IRC.Base
import           Network.IRC.Bot
import           Network.IRC.Bot.Log
import           Network.IRC.Bot.Part.Channels
import           Network.IRC.Bot.Part.Dice
import           Network.IRC.Bot.Part.Hello
import           Network.IRC.Bot.Part.NickUser
import           Network.IRC.Bot.Part.Ping
import           Paths_ribot (version)
import           System.Console.CmdArgs
import           System.IO (appendFile)
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
            configFile <- C.load [ C.Optional "/etc/ribot/ribot.cfg"
                                 , C.Optional "$(HOME)/.ribot.cfg"
                                 , C.Required fileName
                                 ]
            config' <- initConfig configFile

            case config' of
                Nothing     -> putStrLn "You must are missing configuration keys."
                Just config -> runBot configFile config

runBot :: Config -> BotConf -> IO ()
runBot config botConfig = do
    asDaemon <- C.lookupDefault False config "daemonize"
    if asDaemon
        then daemonize runDaemon
        else runConsole
    where
        runConsole :: IO ()
        runConsole = do
            parts <- initParts botConfig
            tids  <- simpleBot botConfig parts
            (logger botConfig) Important "Press ENTER to quit."
            getLine
            mapM_ killThread tids

        runDaemon :: IO ()
        runDaemon =   initParts botConfig
                  >>= simpleBot botConfig
                  >>  forever (threadDelay 100000)

initParts :: (BotMonad m) => BotConf -> IO [m ()]
initParts config = do
    (_, chanParts) <- initChannelsPart $ channels config
    return [ pingPart
           , nickUserPart
           , chanParts
           , dicePart
           , helloPart
           ]

initConfig :: Config -> IO (Maybe BotConf)
initConfig config = getHostName >>= initConfig' config
    where
        initConfig' :: Config -> HostName -> IO (Maybe BotConf)
        initConfig' config hostName = do
            server <- C.lookup config "irc.server"
            port   <- C.lookupDefault 6667 config "irc.port"
            nick   <- C.lookup config "irc.nick"
            chan   <- C.lookup config "irc.channel"
            logger <- getLogger config
            return $ maybeConfig hostName server port nick chan logger

        maybeConfig :: HostName -> Maybe HostName -> Int -> Maybe String
                    -> Maybe String -> Logger -> Maybe BotConf
        maybeConfig host server port nick chan logger = do
            server' <- server
            nick'   <- nick
            chan'   <- chan
            return $ BotConf
                { channelLogger = Just writeMsg
                , logger        = logger
                , host          = server'
                , port          = PortNumber $ fromIntegral port
                , nick          = nick'
                , commandPrefix = "!"
                , user          = User nick' host server' nick'
                , channels      = S.singleton chan'
                }

        getLogger :: Config -> IO Logger
        getLogger config =
            pure getLogger' <*> C.lookupDefault 1 config "log.level"
                            <*> C.lookupDefault "STDOUT" config "log.file"

        getLogger' :: Int -> FilePath -> Logger
        getLogger' 0 _            = nullLogger
        getLogger' level "STDOUT" = stdoutLogger (getLevel level)
        getLogger' level file     = appendLogger (getLevel level) file

        appendLogger :: LogLevel -> FilePath -> Logger
        appendLogger level file msgLevel msg =
            when (msgLevel >= level) (appendFile file $ msg ++ "\n")

        getLevel :: Int -> LogLevel
        getLevel 1 = Important
        getLevel 2 = Network.IRC.Bot.Log.Normal
        getLevel _ = Debug


writeMsg :: Chan Message -> IO ()
writeMsg chan = readChan chan >>= putStrLn . ("MESSAGE: " ++) . show

