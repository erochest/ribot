{-# LANGUAGE OverloadedStrings #-}

-- This contains functions for reading the configuration file and converting
-- the `Config` object into a `BotConf` object.

module Data.Ribot.Config
    ( readConfig
    , createBotConf
    ) where

import           Data.Configurator
import           Data.Configurator.Types (Config)
import qualified Data.Set as S
import           Control.Applicative
import           Control.Concurrent.Chan
import           Control.Monad (forever, when)
import           Network
import           Network.BSD
import           Network.IRC.Base
import           Network.IRC.Bot
import           Network.IRC.Bot.Log
import           Prelude hiding (lookup)
import           System.Directory
import           System.FilePath ((</>))


-- This takes a file name and reads the configuration file.
readConfig :: FilePath -> IO Config
readConfig cliConfigPath = do
    appUserDir <- getAppUserDataDirectory "ribot"
    load [ Optional "/etc/ribot/ribot.cfg"
         , Optional "$(HOME)/.ribot.cfg"
         , Optional $ appUserDir </> "ribot.cfg"
         , Required cliConfigPath
         ]

-- This takes a `Config` value and converts it to a `BotConf`. If any required
-- values are missing from the `Config`, `Nothing` is returned.
createBotConf :: Config -> (Chan Message -> IO ()) -> IO (Maybe BotConf)
createBotConf config channelLogger = getHostName >>= initConfig' config
    where
        initConfig' :: Config -> HostName -> IO (Maybe BotConf)
        initConfig' config hostName = do
            server <- lookup config "irc.server"
            port   <- lookupDefault 6667 config "irc.port"
            nick   <- lookup config "irc.nick"
            chan   <- lookup config "irc.channel"
            logger <- getLogger config
            return $ maybeConfig hostName server port nick chan logger

        maybeConfig :: HostName -> Maybe HostName -> Int -> Maybe String
                    -> Maybe String -> Logger -> Maybe BotConf
        maybeConfig host server port nick chan logger = do
            server' <- server
            nick'   <- nick
            chan'   <- chan
            return $ BotConf
                { channelLogger = Just channelLogger
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
            getLogger' <$> lookupDefault 1 config "log.level"
                       <*> lookupDefault "STDOUT" config "log.file"

        getLogger' :: Int -> FilePath -> Logger
        getLogger' 0 _            = nullLogger
        getLogger' level "STDOUT" = stdoutLogger (getLevel level)
        getLogger' level file     = appendLogger (getLevel level) file

        appendLogger :: LogLevel -> FilePath -> Logger
        appendLogger level file msgLevel msg =
            when (msgLevel >= level) (appendFile file $ msg ++ "\n")

        getLevel :: Int -> LogLevel
        getLevel 1 = Important
        getLevel 2 = Normal
        getLevel _ = Debug


