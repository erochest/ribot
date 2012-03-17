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
-- ### `ribot` Section
--
-- * `daemonize` -- This is a boolean value (`true`, `false`, or `off`, `on`).
-- If `true`, this runs Ribot as a daemon.
-- * `db_file` -- This is the location of the database file.
-- * `pastebin` -- This is the optional PasteBin API key.
-- * `search_max` -- The maximum number of search results to include. This
-- defaults to 10000.
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
-- > ribot
-- > {
-- >   daemonize  = off
-- >   pastebin   = "..."
-- >   db_file    = ":memory:"
-- >   search_max = 1000
-- > }
--
-- ### Daemon Mode
--
-- If daemon mode is specified, the logging needs to be set anywhere other than
-- `STDOUT`; otherwise, logging will go no where.

module Data.Ribot.Config
    ( readConfig
    , createBotConf
    , ircServer
    , ircNick
    , ircChannel
    , logLevel
    , logFile
    , ribotDaemonize
    , ribotPasteBin
    , ribotDbFile
    , ribotSearchMax
    , absPath
    , getLevel
    ) where

import           Data.Configurator
import           Data.Configurator.Types (Config, Configured, Name)
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
import           System.FilePath ((</>), isAbsolute)


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
            maybeConfig hName <$> ircServer cfg
                              <*> ircPort cfg
                              <*> ircNick cfg
                              <*> ircChannel cfg
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
            getLogger' <$> logLevel cfg
                       <*> logFile cfg

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

-- This just defines a swapped version of lookup so I can use pointless style
-- later on.
lookup' :: Configured a => Name -> Config -> IO (Maybe a)
lookup' = flip lookup

-- This just defines a swapped version of lookupDefault so I can use pointless
-- style later on.
lookupDefault' :: Configured a => a -> Name -> Config -> IO a
lookupDefault' def = flip (lookupDefault def)

-- This looks up a file path and returns it. If the default is used, it's
-- returned as-is; otherwise, the path is canonicalized.
lookupFilePath :: FilePath -> Name -> Config -> IO FilePath
lookupFilePath defaultPath name config =
    lookupDefault' defaultPath name config >>= absPath defaultPath

-- This returns the irc.server setting from the configuration.
ircServer :: Config -> IO (Maybe String)
ircServer = lookup' "irc.server"

-- This looks up the irc.port setting from the configuration.
ircPort :: Config -> IO Int
ircPort = lookupDefault' 6667 "irc.port"

-- This looks up the irc.nick setting from the configuration.
ircNick :: Config -> IO (Maybe String)
ircNick = lookup' "irc.nick"

-- This looks up the irc.channel setting from the configuration.
ircChannel :: Config -> IO (Maybe String)
ircChannel = lookup' "irc.channel"

-- This looks up the log.level setting from the configuration, defaulting to 1
-- (Important).
logLevel :: Config -> IO Int
logLevel = lookupDefault' 1 "log.level"

-- This looks up the log.file setting from the configuration, default to
-- "STDOUT." If a file name is given, it's made absolute.
logFile :: Config -> IO FilePath
logFile = lookupFilePath "STDOUT" "log.file"

-- This looks up the ribot.daemonize setting from the configuration, defaulting
-- to False.
ribotDaemonize :: Config -> IO Bool
ribotDaemonize = lookupDefault' False "ribot.daemonize"

-- This looks up the ribot.pastebin setting from the configuration.
ribotPasteBin :: Config -> IO (Maybe String)
ribotPasteBin = lookup' "ribot.pastebin"

-- This looks up the ribot.db_file setting from the configuration, defaulting
-- to ":memory:".
ribotDbFile :: Config -> IO String
ribotDbFile = lookupFilePath ":memory:" "ribot.db_file"

-- This looks up the maximum number of search results to return. This defaults
-- to 10,000.
ribotSearchMax :: Config -> IO Int
ribotSearchMax = lookupDefault' 10000 "ribot.search_max"

-- This takes a log file path and makes it absolute (canonicalizes it).
absPath :: FilePath -> FilePath -> IO FilePath
absPath def path | def == path     = return path
                 | isAbsolute path = return path
                 | otherwise       = (</> path) `fmap` getCurrentDirectory

-- This converts a `log.level` value from the configuration file into a
-- `LogLevel`.
getLevel :: Int -> LogLevel
getLevel 1 = Important
getLevel 2 = Normal
getLevel _ = Debug

