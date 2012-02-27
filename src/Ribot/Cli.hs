
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
    deriving (Data, Show, Typeable)

ribotModes :: Modes
ribotModes = modes
    [ Listen
        { config = def &= name "c" &= help "The location of the configuration file."
        } &= details ["This listens on an IRC channel."] &= auto
    ] &= summary ("ribot v" ++ showVersion version)
      &= program "ribot"


