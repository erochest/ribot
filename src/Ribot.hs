
-- This is a test of using [docco](http://jashkenas.github.com/docco/) on
-- Haskell files.

-- This defines the Main module, which contains the program's entry point.
module Main where

import           Control.Exception (bracket)
import qualified Database.HDBC as Db
import           Data.Time
import           Network
import           Network.Ribot.Irc
import           Database.Ribot (connectDb)
import           Network.Ribot.Search (reindex)
import           Network.Ribot.Types (runNetState, RibotState(..))
import           Ribot.Cli
import           System.IO
import           Text.Printf

-- The main function.
main :: IO ()
main = do
    mode <- cmdArgs ribotModes
    case mode of
        Listen server port chan nick dbFile -> do
            t <- getCurrentTime
            bracket (connect server port chan nick dbFile) disconnect
                    (loop (RibotState t))
        Reindex dbFile -> do
            putStrLn "reindexing database."
            (msgCount, tknCount) <- bracket (connectDb dbFile)
                                            Db.disconnect
                                            reindex
            printf "indexed %d messages; %d tokens.\n" msgCount tknCount
    where
        -- Compose functions to get the bot's socket and close it.
        disconnect = hClose . botSocket

        -- This runs an infinite loop listening for a line from the IRC channel
        -- and processing it.
        loop st cfg = catch (runNetState runRibot cfg st) (const $ return ())

