
-- This is a test of using [docco](http://jashkenas.github.com/docco/) on
-- Haskell files.

-- This defines the Main module, which contains the program's entry point.
module Main where

import           Control.Exception (bracket)
import           Control.Monad.Reader
import           Network
import           Network.Ribot.Core
import           System.Console.CmdArgs
import           System.IO

-- These are the default command-line settings.
defaultServer :: String
defaultServer = "irc.freenode.org"
defaultPort   :: Int
defaultPort   = 6667
defaultChan   :: String
defaultChan   = "#err1234567890"
defaultNick   :: String
defaultNick   = "ribot-bot"

-- These are the command line options.
data Modes
    = Listen
        { server  :: String
        , port    :: Int
        , channel :: String
        , nick    :: String
        }
    deriving (Show, Data, Typeable)

-- Here are the options for the options.
ribotModes :: Modes
ribotModes =
    Listen
      { server = defaultServer &= name "s" &= typ "HOST"
                               &= help ("The IRC server (default is " ++ defaultServer ++ ").")
      , port = defaultPort &= name "p" &= typ "PORT"
                           &= help ("The port of the IRC server (default is " ++ (show defaultPort) ++ ").")
      , channel = defaultChan &= name "c" &= typ "CHANNEL"
                              &= help ("The channel on the server (default is " ++ defaultChan ++ ").")
      , nick = defaultNick &= name "n" &= typ "NICKNAME"
                           &= help ("The IRC nick name (default is " ++ defaultNick ++ ").")
      } &= details ["This listens on an IRC channel."]
    &= summary ("ribot v" ++ ribotVersion)
    &= program "ribot"

-- The main function.
main :: IO ()
main = do
    mode <- cmdArgs ribotModes
    case mode of
        Listen server port chan nick -> 
            bracket (connect server port chan nick) disconnect loop
    where
        -- Compose functions to get the bot's socket and close it.
        disconnect = hClose . botSocket

        -- This runs an infinite loop listening for a line from the IRC channel
        -- and processing it.
        loop st = catch (runReaderT runRibot st) (const $ return ())

