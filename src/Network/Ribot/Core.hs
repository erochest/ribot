
-- These are the core Ribot functions. This also includes the data structure
-- for the Ribot bot, as well as functions to make working with IRC easier.

module Network.Ribot.Core
    ( Ribot (..)
    , Net
    , connect
    , runRibot
    , write
    , clean
    , privmsg
    , ribotVersion
    ) where

import           Control.Exception (bracket_)
import           Control.Monad (mapM_)
import           Control.Monad.Reader
import           Database.HDBC (withTransaction)
import qualified Data.List as L
import qualified Data.Maybe as M
import           Data.Time
import           Network
import           Network.Ribot.Db
import           Network.Ribot.Message
import           Network.Ribot.Search (index, search)
import           Network.Ribot.Utils (split)
import           System.Exit
import           System.IO
import           System.Locale
import           Text.Printf

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

-- This is the main data structure for the bot. It has connection information,
-- connection handles for IRC and the database, and the time the bot started
-- (for `!uptime`).
data Ribot = Ribot { botSocket    :: Handle
                   , botServer    :: String
                   , botPort      :: Int
                   , botChan      :: String
                   , botNick      :: String
                   , botStartTime :: UTCTime
                   , botDbHandle  :: ConnWrapper
                   }

-- This is the monad the bot runs under. The `ReaderT` holds the state (a
-- `Ribot`) and silently threads it through the computation.
type Net = ReaderT Ribot IO

-- This connects to IRC, to the database, notes the current time, and returns a
-- ready-to-go `Ribot`.
connect :: String -> Int -> String -> String -> Maybe String -> IO Ribot
connect server port chan nick dbFile = notify $ do
    h <- connectTo server . PortNumber $ fromIntegral port
    t <- getCurrentTime
    hSetBuffering h NoBuffering
    db <- connectDb dbFile
    return $ Ribot h server port chan nick t db
    where
        -- This prints some information to the screen about what we're doing.
        -- This should probably refactored to make it more functional.
        notify a = bracket_
            (printf "Connecting to %s ..." server >> hFlush stdout)
            (putStrLn "done.")
            a

-- Run in Net. This is actually executed by `runReaderT`.
--
-- First, `runRibot` logs onto the server and channel, then it listens.
runRibot :: Net ()
runRibot = do
    n <- asks botNick
    write "NICK" n
    write "USER" (n ++ " 0 * :riBOT")
    asks botChan >>= write "JOIN"
    asks botSocket >>= listen

-- This is a shortcut for `liftIO` in the context of a `Net` monad.
io :: IO a -> Net a
io = liftIO

-- This writes a command and string to IRC. It also prints them to the screen.
write :: String -> String -> Net ()
write s t = do
    h <- asks botSocket
    io $ hPrintf h "%s %s\r\n" s t
    io $ printf    "> %s %s\n" s t

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
    s <- init `fmap` io (hGetLine h)
    io $ logInput s
    case s of
        ping | "PING" `L.isPrefixOf` ping -> do
            write "PONG" ""
            return ()
        otherwise -> do
            msg <- io (parseMessage s)
            -- io (putStrLn $ show msg)
            eval msg
    where
        -- `forever` executes a and then recursively executes it again. Only
        -- Chunk Norris can stop it. (And Ctrl-C, which may stand for
        -- Ctrl-Chuck Norris, although nothing can control Chunk Norris.)
        --
        -- I should probably use the one from Control.Monad, but I've already
        -- written the comment above.
        forever a = a >> forever a

-- This cleans up a string send by IRC by removing the prefix.
clean :: String -> String
clean = drop 1 . dropWhile (/= ':') . drop 1

-- This evaluates the input and runs commands based on it. This is also where
-- you'd add new commands.
--
-- Commands it recognizes now are:
--
-- * `!version` — print out the version of the bot.
-- * `!uptime` — print how long the bot has been running.
-- * `!log off` — turn off logging for the user.
-- * `!log on` — turn on logging for the user.
-- * `!echo NAME` — echo back.
eval :: Message -> Net ()
eval (Message _ _ _ "!help") =
    mapM_ privmsg helpMessage
eval (Message _ _ _ "!version") =
    privmsg $ "version " ++ ribotVersion
eval (Message _ _ _ "!uptime") =
    uptime >>= privmsg
eval (Message (Just usr) _ _ log) | "!log " `L.isPrefixOf` log = do
    db <- asks botDbHandle
    io . withTransaction db $ \cxn ->
        setUserLogging cxn usr logFlag
    privmsg $ "Logging turned " ++ logMsg ++ " for " ++ usr ++ "."
    where logFlag = log == "!log on"
          logMsg = if logFlag then "on" else "off"
eval (Message _ _ _ x) | "!echo" `L.isPrefixOf` x =
    privmsg (drop 6 x)
eval (Message _ _ _ x) | "!search" `L.isPrefixOf` x = do
    db <- asks botDbHandle
    results <- io $ search db query
    mapM_ privmsg . map showSearchResult $ results
    where query = drop 8 x
          showSearchResult (_, nick, date, msg) =
            "[" ++ date ++ "] " ++ nick ++ ": " ++ msg ++ "\n"
eval (Message _ _ _ ('!':_)) = return ()
eval msg = processMessage msg

-- This gets how long the bot has been running and returns it as a string.
uptime :: Net String
uptime = do
    now <- io getCurrentTime
    zero <- asks botStartTime
    return . show $ diffUTCTime now zero

-- This sends a privmsg to the channel the bot's in.
privmsg :: String -> Net ()
privmsg s = do
    asks botChan >>= \c ->
        write "PRIVMSG" $ c ++ " :" ++ s

-- This processes an incoming message.
--
-- Currently, this means that we log it to the database.
--
-- At some point, this should spin off a new thread.
processMessage :: Message -> Net ()
processMessage msg = asks botDbHandle >>= io . (flip withTransaction) process
    where
        process cxn = do
            mId <- logMessage msg cxn
            case mId of
                Just mId' ->
                    index cxn (M.fromMaybe "" $ msgUser msg) mId' $ msgText msg
                Nothing   -> return []
            return ()

