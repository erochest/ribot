
module Main where

import           Control.Exception (bracket)
import           Control.Monad.Reader
import           Network
import           Network.Ribot.Core
import           System.IO

server = "irc.freenode.org"
port   = 6667
chan   = "#err1234567890"
nick   = "ribot-bot"

main :: IO ()
main = bracket (connect server port chan nick) disconnect loop
    where
        disconnect = hClose . botSocket
        loop st = catch (runReaderT runRibot st) (const $ return ())

