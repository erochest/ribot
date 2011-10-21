
-- This is a test of using [docco](http://jashkenas.github.com/docco/) on
-- Haskell files.

-- This defines the Main module, which contains the program's entry point.
module Main where

import           Control.Exception (bracket)
import           Control.Monad.Reader
import           Network
import           Network.Ribot.Core
import           System.IO

-- IRC connection settings.
server = "irc.freenode.org"
port   = 6667
chan   = "#err1234567890"       -- #err1234567890 is an ad hoc testing channel.
nick   = "ribot-bot"

-- The main function.
main :: IO ()
main = bracket (connect server port chan nick) disconnect loop
    where
        -- Compose functions to get the bot's socket and close it.
        disconnect = hClose . botSocket

        -- This runs an infinite loop listening for a line from the IRC channel
        -- and processing it.
        loop st = catch (runReaderT runRibot st) (const $ return ())

