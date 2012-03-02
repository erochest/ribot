{-# LANGUAGE OverloadedStrings #-}

-- This contains functions for reading the configuration file and converting
-- the `Config` object into a `BotConf` object.
--
-- ## Configuration Files
--
-- Configuration files for Ribot are plain text files. They have two sections.
-- Each section has a header and a body, enclosed in curly braces.
--
-- ### `irc` Section
--
-- This contains information for connecting to IRC. It has these keys.
--
-- * `server` -- The address of the IRC server;
-- * `port` -- The IRC server's port (optional);
-- * `nick` -- The nickname to use connecting to IRC; and
-- * `channel` -- The channel to connect to.
--
-- ### `log` Section
--
-- * `level` -- The level of logging. This ranges from 0 (no logging) to 3
-- (every message logged);
-- * `file` -- The file to log to. This can be "STDOUT".
--
-- ### Miscellaneous Options
--
-- Values here don't need to be in any section.
--
-- * `daemonize` -- This is a boolean value (`true`, `false`, or `off`, `on`).
-- If `true`, this runs Ribot as a daemon.
-- * `pastebin` -- This is the PasteBin API key.
--
-- ### Example
--
-- > irc
-- > {
-- >   server = "irc.freenode.org"
-- >   nick   = "ribot"
-- >   channel = "#testchannel"
-- > }
-- > log
-- > {
-- >   level = 2
-- >   file  = STDOUT
-- > }
-- > daemonize = off
-- > pastebin  = "..."
--
-- ### Daemon Mode
--
-- If daemon mode is specified, the logging needs to be set anywhere other than
-- `STDOUT`; otherwise, logging will go no where.

module Data.Ribot.Config
    ( readConfig
    , createBotConf
    ) where

import           Data.Configurator
import           Data.Configurator.Types (Config)
import qualified Data.Set as S
import           Control.Applicative
import           Control.Concurrent.Chan
import           Control.Monad (when)
import           Network
import           Network.BSD
import           Network.IRC.Base
import           Network.IRC.Bot
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
createBotConf config chanLog = getHostName >>= createConf config
    where

        -- This removes the IO context from the parameters, maybe creates a
        -- `BotConf` and re-inserts it into the IO context.
        createConf :: Config -> HostName -> IO (Maybe BotConf)
        createConf cfg hName =
            maybeConfig hName <$> lookup cfg "irc.server"
                              <*> lookupDefault 6667 cfg "irc.port"
                              <*> lookup cfg "irc.nick"
                              <*> lookup cfg "irc.channel"
                              <*> getLogger cfg

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
        maybeConfig hName server portNo nickName chan lg = do
            server' <- server
            nick'   <- nickName
            chan'   <- chan
            return $ BotConf
                { channelLogger = Just chanLog
                , logger        = lg
                , host          = server'
                , port          = PortNumber $ fromIntegral portNo
                , nick          = nick'
                , commandPrefix = "!"
                , user          = User nick' hName server' nick'
                , channels      = S.singleton chan'
                }

        -- This parses out the configuration options for the logger and returns
        -- it.
        getLogger :: Config -> IO Logger
        getLogger cfg =
            getLogger' <$> lookupDefault 1 cfg "log.level"
                       <*> (lookupDefault "STDOUT" cfg "log.file" >>= absPath)

        -- This takes a log file path and makes it absolute (canonicalizes it).
        absPath :: FilePath -> IO FilePath
        absPath "STDOUT" = return "STDOUT"
        absPath path     = canonicalizePath path

        -- This takes the configuration options and turns them into a concrete
        -- `Logger`.
        getLogger' :: Int -> FilePath -> Logger
        getLogger' 0 _            = nullLogger
        getLogger' level "STDOUT" = stdoutLogger (getLevel level)
        getLogger' level file     = appendLogger (getLevel level) file

        -- This `Logger` appends the input line to the file, if its level
        -- matches or is greater than the level this was created with.
        appendLogger :: LogLevel -> FilePath -> Logger
        appendLogger level file msgLevel msgText =
            when (msgLevel >= level) (appendFile file $ msgText ++ "\n")

        -- This converts a `log.level` value from the configuration file into a
        -- `LogLevel`.
        getLevel :: Int -> LogLevel
        getLevel 1 = Important
        getLevel 2 = Normal
        getLevel _ = Debug


