
-- This contains the CLI settings for the bot.

module Ribot.Cli
    ( ribotModes
    , Modes(..)
    ) where


import           Data.Version (showVersion)
import           Paths_ribot (version)
import           System.Console.CmdArgs

-- Modes for the CLI.
data Modes
    = Listen
        { config :: Maybe FilePath
        }
    | Search
        { config :: Maybe FilePath
        , terms  :: [String]
        }
    | Topic
        { config :: Maybe FilePath
        , terms  :: [String]
        }
    | Reindex
        { config :: Maybe FilePath
        }
    deriving (Data, Show, Typeable)

ribotModes :: Modes
ribotModes = modes
    [ Listen
        { config = def &= name "c" &= help "The location of the configuration file."
        } &= details ["This listens on an IRC channel."] &= auto
    , Search
        { config = def &= name "c" &= help "The location of the configuration file."
        , terms = def &= args &= typ "SEARCH-TERMS"
        } &= details ["This searches the messages."]
    , Topic
        { config = def &= name "c" &= help "The location of the configuration file."
        , terms = def &= args &= typ "SEARCH-TERMS"
        } &= details ["This searches the topics."]
    , Reindex
        { config = def &= name "c" &= help "The location of the configuration file."
        } &= details ["This reindexes the messages and topics currently in the database."]
    ] &= summary ("ribot v" ++ showVersion version)
      &= program "ribot"


