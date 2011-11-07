
-- This contains the CLI option processing options for Ribot.

module Ribot.Cli (
      Modes(..)
    , ribotModes
    , cmdArgs
    ) where

import           Network.Ribot.Core (ribotVersion)
import           System.Console.CmdArgs

-- These are the default command-line settings.
defaultServer :: String
defaultServer = "irc.freenode.org"
defaultPort   :: Int
defaultPort   = 6667
defaultChan   :: String
defaultChan   = "#err1234567890"
defaultNick   :: String
defaultNick   = "ribot-bot"
defaultDbFile :: Maybe String
defaultDbFile = Nothing

-- These are the command line options.
data Modes
    = Listen
        { server  :: String
        , port    :: Int
        , channel :: String
        , nick    :: String
        , dbFile  :: Maybe String
        }
    | Reindex
       { dbFile   :: Maybe String
       }
    deriving (Show, Data, Typeable)

-- Here are the options for the options.
ribotModes :: Modes
ribotModes = modes
    [ Listen
        { server = defaultServer &= name "s" &= typ "HOST"
                                 &= help ("The IRC server (default is " ++ defaultServer ++ ").")
        , port = defaultPort &= name "p" &= typ "PORT"
                             &= help ("The port of the IRC server (default is " ++ (show defaultPort) ++ ").")
        , channel = defaultChan &= name "c" &= typ "CHANNEL"
                                &= help ("The channel on the server (default is " ++ defaultChan ++ ").")
        , nick = defaultNick &= name "n" &= typ "NICKNAME"
                             &= help ("The IRC nick name (default is " ++ defaultNick ++ ").")
        , dbFile = defaultDbFile &= name "d" &= typ "DATABASE-FILE"
                                 &= help "The database file to use for logging."
        } &= details ["This listens on an IRC channel."] &= auto
    , Reindex
        { dbFile = defaultDbFile &= name "d" &= typ "DATABASE-FILE"
                                 &= help "The database file to use for logging."
        } &= details ["This reindexes the messages for searching."]
    ] &= summary ("ribot v" ++ ribotVersion)
      &= program "ribot"


