
-- This contains the CLI option processing options for Ribot.
--
-- For more information on the command-line parser and the syntax and capabilities of the Modes below, see the documentation for the cmdargs package, especially these:
--
-- * [On Hackage](http://hackage.haskell.org/package/cmdargs)
-- * [CmdArgs: Easy Command Line Processing](http://community.haskell.org/~ndm/darcs/cmdargs/cmdargs.htm)
-- * [Using CmdArgs (Single and Multi-Mode)](http://zuttobenkyou.wordpress.com/2011/04/19/haskell-using-cmdargs-single-and-multi-mode/)
-- * [Building a Haskell CLI Utility with CmdArgs](http://michaelxavier.net/Building-a-Haskell-CLI-Utility-with-CmdArgs.html)

module Ribot.Cli (
      Modes(..)
    , ribotModes
    , cmdArgs
    ) where

import           System.Console.CmdArgs
-- [Network.Ribot.Irc](../Network/Ribot/Irc.html)
import           Network.Ribot.Irc (ribotVersion)

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
        { server      :: String
        , port        :: Int
        , channel     :: String
        , nick        :: String
        , dbFile      :: Maybe String
        , pasteBinKey :: Maybe String
        , daemonMode  :: Bool
        }
    | Reindex
       { dbFile   :: Maybe String
       }
    | Search
       { dbFile   :: Maybe String
       , terms    :: [String]
       }
    | Mimic
       { dbFile   :: Maybe String
       , nick     :: String
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
        , pasteBinKey = def &= name "k" &= typ "PASTEBIN-API-KEY"
                            &= help ("The developer API key for http://pastebin.com/api. If given, this will be used for long search results.")
        , daemonMode = False &= name "D" &= name "daemon"
                             &= help "A flag to indicate that this should be run in daemon mode."
        } &= details ["This listens on an IRC channel."] &= auto
    , Reindex
        { dbFile = defaultDbFile &= name "d" &= typ "DATABASE-FILE"
                                 &= help "The database file to use for logging."
        } &= details ["This reindexes the messages for searching."]
    , Search
        { dbFile = defaultDbFile &= name "d" &= typ "DATABASE-FILE"
        , terms = def &= args &= typ "SEARCH-TERMS"
        } &= details ["The terms to search for."]
    , Mimic
        { dbFile = defaultDbFile &= name "d" &= typ "DATABASE-FILE"
        , nick = def &= args &= typ "IRC-NICK"
        } &= details ["The user nick to mimic."]
    ] &= summary ("ribot v" ++ ribotVersion)
      &= program "ribot"


