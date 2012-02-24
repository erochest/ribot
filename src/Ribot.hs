{-# LANGUAGE OverloadedStrings #-}

-- This is the entry point for Ribot.

module Main where

import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.Chan
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
                Just config -> do
                    parts  <- initParts config
                    tids   <- simpleBot config parts
                    (logger config) Important "Press ENTER to quit."
                    getLine
                    mapM_ killThread tids

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
            return $ maybeConfig hostName server port nick chan

        maybeConfig :: HostName -> Maybe HostName -> Int -> Maybe String
                    -> Maybe String -> Maybe BotConf
        maybeConfig host server port nick chan = do
            server' <- server
            nick'   <- nick
            chan'   <- chan
            return $ BotConf
                { channelLogger = Just writeMsg
                , logger        = stdoutLogger Debug
                , host          = server'
                , port          = PortNumber $ fromIntegral port
                , nick          = nick'
                , commandPrefix = "!"
                , user          = User nick' host server' nick'
                , channels      = S.singleton chan'
                }

writeMsg :: Chan Message -> IO ()
writeMsg chan = readChan chan >>= putStrLn . ("MESSAGE: " ++) . show

