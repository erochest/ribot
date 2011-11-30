
-- This is a test of using [docco](http://jashkenas.github.com/docco/) on
-- Haskell files.

-- This defines the Main module, which contains the program's entry point.
module Main where

import           Control.Exception (bracket)
import qualified Database.HDBC as Db
import qualified Data.List as L
import           Data.Time
import           Network
import           Ribot.Cli
import           System.IO
import           Text.Printf
-- [Database.Ribot](Database/Ribot.html) <br />
-- [Network.Ribot.Irc](Network/Ribot/Irc.html) <br />
-- [Network.Ribot.Search](Network/Ribot/Search.html) <br />
-- [Network.Ribot.Types](Network/Ribot/Types.html) <br />
-- [Text.Ribot.Generate](Text/Ribot/Generate.html)
import           Database.Ribot (connectDb, getUserMessages, resolveDbFile)
import           Network.Ribot.Irc
import           Network.Ribot.Search (reindex, search, showSearchResult)
import           Network.Ribot.Types
import           System.Log.Handler.Simple
import           System.Log.Logger
import           System.Posix.Daemonize
import           Text.Ribot.Generate (mimic)

-- The main function.
--
-- This parses the command line arguments and handles each mode appropriately.
main :: IO ()
main = do
    mode <- cmdArgs ribotModes
    case mode of
        -- This is the default mode. It listens on IRC. This is the primary
        -- bot mode.
        Listen server port chan nick dbFile pasteBinKey daemon debug -> do
            let runBot = withSocketsDo $ do
                bracket (connect server port chan nick dbFile pasteBinKey)
                        disconnect
                        (uncurry loop)
            case debug of
                True  -> updateGlobalLogger rootLoggerName (setLevel DEBUG)
                False -> return ()
            case daemon of
                True  -> do
                    s <- fileHandler "/tmp/ribot.log" DEBUG
                    updateGlobalLogger rootLoggerName (addHandler s)
                    daemonize runBot
                False -> runBot
        -- This rebuilds the inverted index of messages without connecting to
        -- IRC.
        Reindex dbFile -> do
            putStrLn "reindexing database."
            (msgCount, tknCount) <- bracket (resolveDbFile dbFile >>= connectDb)
                                            Db.disconnect
                                            reindex
            printf "indexed %d messages; %d tokens.\n" msgCount tknCount
        -- This searches the messages without connecting to IRC.
        Search dbFile terms -> do
            bracket (resolveDbFile dbFile >>= connectDb)
                    Db.disconnect
                    (search' terms)
        -- This runs the `!mimic` command from the command line.
        Mimic dbFile nick -> do
            bracket (resolveDbFile dbFile >>= connectDb)
                    Db.disconnect
                    (mimic' nick)
    where
        -- Compose functions to get the bot's socket and close it.
        disconnect = hClose . botSocket . snd

        -- This runs an infinite loop listening for a line from the IRC channel
        -- and processing it.
        loop cfg st = catch (runNet runRibot cfg st) (const $ return ())

        -- This performs a search on the command line.
        search' :: Db.IConnection c => [String] -> c -> IO ()
        search' terms cxn = do
            results <- search cxn query
            mapM_ (putStrLn . showSearchResult) results
            printf "Found %d message(s).\n" $ length results
            where query = L.intercalate " " terms

        -- This mimics the nick given.
        mimic' :: Db.IConnection c => String -> c -> IO ()
        mimic' nick cxn = do
            messages <- return . map snd =<< getUserMessages cxn nick
            tokens   <- mimic messages 12
            putStrLn . ("> " ++) $ L.intercalate " " tokens

