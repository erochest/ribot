
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
import           Database.Ribot (initTempTable, resolveDbFile)
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
-- [Network.Ribot.Types](Types.html) <br />
-- [Network.Ribot.Search](Search.html) <br />
-- [Text.Ribot.Utils](../../Text/Ribot/Utils.html)
import           Database.Ribot
import           Network.Ribot.Types
import           Network.Ribot.Search (index, search)
import           Text.Ribot.Utils (split)

-- This is the version, for the command line and the !version command.
ribotVersion :: String
ribotVersion =  "0.2.0"

-- This is the help message for the bot.
helpMessage :: [String]
helpMessage =
    [ "I understand these commands:"
    , "!help: Print out this information."
    , "!version: Print out my version."
    , "!uptime: Print out how many seconds I've been running without taking a break."
    , "!log off: Stop logging your messages."
    , "!log on: Start logging your messages."
    , "!echo STRING: Right back at'cha."
    , "!search QUERY: Search the logs for one or more terms."
    ]


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
        hPrintf h "%s\r\n" output
        printf "> %s\n" output
    return $ RibotState h t db $ writeChan out

-- This reconnects to the IRC server. It maintains the same connection to the
-- database and other settings.
reconnectIRC :: Net ()
reconnectIRC = do
    state  <- get
    io . hClose $ botSocket state
    server <- asks botServer
    port   <- asks botPort
    h      <- io . connectTo server . PortNumber $ fromIntegral port
    io $ hSetBuffering h NoBuffering
    login
    put $ state { botSocket=h }

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

-- This is a shortcut for `liftIO` in the context of a `Net` monad.
io :: IO a -> Net a
io = liftIO

-- This writes a command and string to IRC. It also prints them to the screen.
write :: String -> String -> Net ()
write s t = do
    output <- return . botOutput =<< get
    io . output $ s ++ " " ++ t

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

-- This evaluates the input and runs commands based on it. This is also where
-- you'd add new commands.
--
-- Commands it recognizes now are:
eval :: Message -> Net ()
-- * `!help` — print this message;
eval (Message _ _ _ "!help") =
    mapM_ privmsg helpMessage
-- * `!version` — print out the version of the bot;
eval (Message _ _ _ "!version") =
    privmsg $ "version " ++ ribotVersion
-- * `!uptime` — print how long the bot has been running;
eval (Message _ _ _ "!uptime") =
    uptime >>= privmsg
-- * `!log off` — turn off logging for the user;
-- * `!log on` — turn on logging for the user;
eval (Message (Just usr) _ _ log) | "!log " `L.isPrefixOf` log = do
    db <- return . botDbHandle =<< get
    io . withTransaction db $ \cxn ->
        setUserLogging cxn usr logFlag
    privmsg $ "Logging turned " ++ logMsg ++ " for " ++ usr ++ "."
    where logFlag = log == "!log on"
          logMsg = if logFlag then "on" else "off"
-- * `!echo NAME` — echo back;
eval (Message _ _ _ x) | "!echo" `L.isPrefixOf` x =
    privmsg (drop 6 x)
-- * `!search TERMS` — This searches the log and prints out the last 25
--   results;
eval (Message _ _ _ x) | "!search" `L.isPrefixOf` x = do
    db <- return . botDbHandle =<< get
    results <- io $ search db query
    mapM_ privmsg . map showSearchResult $ results
    privmsg $ show (length results) ++ " message(s) found."
    where query = drop 8 x
          showSearchResult (_, nick, date, msg) =
            "[" ++ date ++ "] " ++ nick ++ ": " ++ msg ++ "\n"
-- * All other `!` commands are ignored; and
eval (Message _ _ _ ('!':_)) = return ()
-- * All other messages are handled by `processMessage`.
eval msg = processMessage msg

-- This gets how long the bot has been running and returns it as a string.
uptime :: Net String
uptime = do
    now <- io getCurrentTime
    zero <- return . botStartTime =<< get
    return . show $ diffUTCTime now zero

-- This sends a privmsg to the channel the bot's in.
privmsg :: String -> Net ()
privmsg s = do
    asks botChan >>= \c ->
        write "PRIVMSG" $ c ++ " :" ++ s

-- This processes an incoming message.
--
-- Currently, this means that we log it to the database and add its tokens to
-- the inverted index.
processMessage :: Message -> Net ()
processMessage msg = do
    (RibotState _ _ db _) <- get
    io . withTransaction db $ \cxn -> do
        mId <- logMessage msg cxn
        case mId of
            Just mId' ->
                index cxn (M.fromMaybe "" $ msgUser msg) mId' $ msgText msg
            Nothing   -> return []
        return ()

-- This checks that a user exists in the database, and inserts it if necessary.
checkUser :: IConnection c => c -> String -> IO ()
checkUser cxn nick =
    run cxn "INSERT OR IGNORE INTO user \
            \ (username, logging_on) VALUES \
            \ (?, 1);"
            [toSql nick] >>
    return ()

-- This saves a message to the database. This doesn't handle the transaction.
-- You probably want to do that at a higher level.
--
-- This returns the ID of the message in the `message` database or Nothing.
logMessage :: IConnection c => Message -> c -> IO (Maybe Int)
logMessage msg cxn = do
    -- If there's no user, move on.
    case (msgUser msg) of
        Just userName -> addMsg userName $ msgText msg
        Nothing       -> return Nothing

    where
        addMsg :: String -> String -> IO (Maybe Int)
        addMsg userName msgStr = do
            -- First, try to create the user, if the name isn't already in the db.
            checkUser cxn userName
            -- Second, add the message to the database.
            run cxn "INSERT INTO message (user_id, text, posted) \
                \ SELECT id, ?, DATETIME('NOW') \
                \ FROM user WHERE username=? AND logging_on=1;"
                [toSql msgStr, toSql userName]
            msgId <- quickQuery' cxn "SELECT last_insert_rowid();" []
            return $ case msgId of
                [[sqlId]] -> Just $ fromSql sqlId
                _         -> Nothing

-- This sets the user.logging_on value for the given user.
setUserLogging :: IConnection c => c -> String -> Bool -> IO ()
setUserLogging cxn nick loggingOn = do
    checkUser cxn nick
    run cxn "UPDATE user \
            \ SET logging_on=? \
            \ WHERE username=?;"
        [iToSql loggingInt, toSql nick]
    return ()
    where loggingInt = if loggingOn then 1 else 0


