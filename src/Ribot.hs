
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
import           Network.Ribot.Types
import           Ribot.Cli
import           System.IO
import           Text.Printf

-- The main function.
main :: IO ()
main = do
    mode <- cmdArgs ribotModes
    case mode of
        Listen server port chan nick dbFile -> do
            bracket (connect server port chan nick dbFile) disconnect
                    (uncurry loop)
        Reindex dbFile -> do
            putStrLn "reindexing database."
            (msgCount, tknCount) <- bracket (connectDb dbFile)
                                            Db.disconnect
                                            reindex
            printf "indexed %d messages; %d tokens.\n" msgCount tknCount
    where
        -- Compose functions to get the bot's socket and close it.
        disconnect = hClose . botSocket . snd

        -- This runs an infinite loop listening for a line from the IRC channel
        -- and processing it.
        loop cfg st = catch (runNet runRibot cfg st) (const $ return ())

