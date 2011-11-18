
-- These are the core Ribot functions. This also includes the data structure
-- for the Ribot bot, as well as functions to make working with IRC easier.

module Network.Ribot.Irc
    ( Ribot (..)
    , Net
    , connect
    , runRibot
    , write
    , clean
    , privmsg
    , ribotVersion
    ) where

import           Control.Concurrent (forkIO)
import           Control.Concurrent.Chan
import           Control.Exception (bracket_)
import           Control.Monad (mapM_, forever, liftM)
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans (lift)
import           Database.HDBC
import qualified Data.List as L
import qualified Data.Maybe as M
import           Data.Time
import           Network
import           System.Exit
import           System.IO
import           System.Locale
import           System.Timeout
import           Text.Printf

-- [Database.Ribot](../../Database/Ribot.html) <br />
-- [Network.Ribot.Irc.Commands](./Irc/Commands.html) <br />
-- [Network.Ribot.Types](Types.html) <br />
-- [Network.Ribot.Search](Search.html) <br />
-- [Text.Ribot.Utils](../../Text/Ribot/Utils.html)
import           Database.Ribot
import           Network.Ribot.Irc.Commands
import           Network.Ribot.Types
import           Network.Ribot.Search (index, search, showSearchResult)
import           Text.Ribot.Utils (split)

-- This is the maximum time that we can go without hearing from the server. If
-- this timeout is reached, we re-connect.  Currently, this is set for five
-- minutes.
timeoutPeriod :: Int
timeoutPeriod =  5 * 60 * 10^6

-- This takes the ribot and fills in the state by connecting to IRC,
-- the database, etc.
connectState :: Ribot -> IO RibotState
connectState (Ribot server port chan nick dbFile) = do
    t <- getCurrentTime
    -- First, connect to IRC and set the buffering.
    printf "Connecting to %s:%d..." server port
    h <- connectTo server . PortNumber $ fromIntegral port
    hSetBuffering h NoBuffering
    -- Second, connect to the database.
    db <- connectDb dbFile
    -- Set up a channel to send output to and fork a thread to listen to the
    -- channel and write everything sent across it to STDOUT and IRC.
    out <- newChan
    forkIO . forever $ do
        output <- readChan out
        printf "> %s\n" output
        hPrintf h "%s\r\n" output
    return $ RibotState h t db $ writeChan out

-- This disconnects a Ribot state from all connects (just database and IRC at
-- this point). It leaves the state itself intact, but using it will generate
-- errors.
disconnectState :: Net ()
disconnectState = do
    gets botSocket   >>= io . hClose
    gets botDbHandle >>= io . disconnect

-- This reconnects to the IRC server. It maintains the same connection to the
-- database and other settings.
reconnectIRC :: Net ()
reconnectIRC = do
    disconnectState
    put =<< io . connectState =<< ask
    login

-- This connects to IRC, to the database, notes the current time, and returns a
-- ready-to-go `Ribot`.
connect :: String -> Int -> String -> String -> Maybe FilePath -> IO (Ribot, RibotState)
connect server port chan nick dbFile = do
    dbFile' <- resolveDbFile dbFile
    let ribot = Ribot server (fromIntegral port) chan nick dbFile'
    state <- connectState ribot
    return (ribot, state)

-- Run in `Net`.
--
-- First, `runRibot` logs onto the server and channel, then it listens.
runRibot :: Net ()
runRibot = do
    login
    get >>= listen . botSocket

-- This logs onto IRC with a nick into a channel.
login :: Net ()
login = do
    gets botSocket >>= io . hShow >>= io . printf "Logging onto %s\n"
    nick <- asks botNick
    write "NICK" nick
    write "USER" (nick ++ " 0 * :riBOT")
    asks botChan >>= write "JOIN"

-- This is a stripped-down version of `runRibot`. It doesn't log into the
-- server or listen to anything. It just evaluates a string in the context of
-- some `Ribot` data.
--
-- Because this assumes that it's being run in another thread, possibly another
-- OS thread, it creates a new database connection.
evalRibot :: Ribot -> RibotState -> Message -> IO ()
evalRibot ribot state input = do
    db <- clone $ botDbHandle state
    withTransaction db initTempTable
    initTempTable db
    runNet (eval input) ribot $ state { botDbHandle=db }

-- This writes the input line to the screen with a timestamp.
logInput :: String -> IO ()
logInput input = do
    dt <- getCurrentTime
    let time = formatTime defaultTimeLocale "%c" dt
    printf "[%s] %s\n" time input

-- This listens forever. It pulls a line from IRC, prints it, cleans it up, and
-- evaluates it.
listen :: Handle -> Net ()
listen h = forever $ do
    -- Listening to the server can time out. If it does, we have to reconnect.
    maybeS <- io . timeout timeoutPeriod $ init `fmap` liftIO (hGetLine h)
    case maybeS of
        Nothing -> do
            io $ putStrLn "Lost connection. Re-connecting..."
            reconnectIRC
            io $ putStrLn "OK. Reconnected..."
        Just s -> handleInput s

-- This processes a line of input from the IRC server.
handleInput :: String -> Net ()
handleInput s = do
    io $ logInput s
    case s of
        -- The server can send the `PING` command, to which we have to
        -- immediately reply "PONG".
        ping | "PING" `L.isPrefixOf` ping -> do
            write "PONG" ""
            return ()

        -- Otherwise, we parse the message, get the configuration and state to
        -- the monad can be recreated in a second thread, and fork the new
        -- thread to evaluate the input.
        otherwise -> do
            msg   <- io (parseMessage s)
            ribot <- ask
            state <- get
            _     <- io . forkIO $ evalRibot ribot state msg
            return ()

-- This cleans up a string send by IRC by removing the prefix.
clean :: String -> String
clean = drop 1 . dropWhile (/= ':') . drop 1

