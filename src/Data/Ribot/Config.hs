{-# LANGUAGE OverloadedStrings #-}

-- This contains functions for reading the configuration file and converting
-- the `Config` object into a `BotConf` object.

module Data.Ribot.Config
    ( readConfig
    , createBotConf
    ) where

import           Control.Applicative
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
createBotConf config channelLogger = getHostName >>= createConf config
    where

        -- This removes the IO context from the parameters, maybe creates a
        -- `BotConf` and re-inserts it into the IO context.
        createConf :: Config -> HostName -> IO (Maybe BotConf)
        createConf config hostName =
            maybeConfig hostName <$> lookup config "irc.server"
                                 <*> lookupDefault 6667 config "irc.port"
                                 <*> lookup config "irc.nick"
                                 <*> lookup config "irc.channel"
                                 <*> getLogger config

        -- This takes the parameters, some maybe not provided, and returns a
        -- `BotConf`. If any of the parameters wrapped in `Maybe` aren't given
        -- (i.e., are `Nothing`), then return `Nothing`.
        maybeConfig :: HostName             -- Client host
                    -> Maybe HostName       -- IRC server
                    -> Int                  -- IRC port
                    -> Maybe String         -- Nickname
                    -> Maybe String         -- Channel
                    -> Logger               -- logger
                    -> Maybe BotConf        -- Configuration
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

        -- This parses out the configuration options for the logger and returns
        -- it.
        getLogger :: Config -> IO Logger
        getLogger config =
            getLogger' <$> lookupDefault 1 config "log.level"
                       <*> lookupDefault "STDOUT" config "log.file"

        -- This takes the configuration options and turns them into a concrete
        -- `Logger`.
        getLogger' :: Int -> FilePath -> Logger
        getLogger' 0 _            = nullLogger
        getLogger' level "STDOUT" = stdoutLogger (getLevel level)
        getLogger' level file     = appendLogger (getLevel level) file

        -- This `Logger` appends the input line to the file, if its level
        -- matches or is greater than the level this was created with.
        appendLogger :: LogLevel -> FilePath -> Logger
        appendLogger level file msgLevel msg =
            when (msgLevel >= level) (appendFile file $ msg ++ "\n")

        -- This converts a `log.level` value from the configuration file into a
        -- `LogLevel`.
        getLevel :: Int -> LogLevel
        getLevel 1 = Important
        getLevel 2 = Normal
        getLevel _ = Debug


