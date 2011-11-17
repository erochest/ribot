
-- Has the `eval` function and the definitions of the IRC `!` commands.

module Network.Ribot.Irc.Commands
    ( eval
    , ribotVersion
    , privmsg
    , write
    ) where

import           Control.Monad (mapM_, liftM)
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.List as L
import qualified Data.Maybe as M
import           Data.Time
import           Database.HDBC
-- [Database.Ribot](../../../Database/Ribot.html) <br />
-- [Network.Ribot.Types](../Types.html) <br />
-- [Network.Ribot.Search](../Search.html) <br />
-- [Text.Ribot.Generate](../../../Text/Ribot/Generate.html) <br />
import           Database.Ribot (getUserMessages)
import           Network.Ribot.Search (index, search, showSearchResult)
import           Network.Ribot.Types
import           Text.Ribot.Generate (mimic)

-- This is the version, for the command line and the !version command.
ribotVersion :: String
ribotVersion =  "0.3.0"

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
    , "!mimic USER: Return a short string that mimics a user."
    ]

-- This sends a privmsg to the channel the bot's in.
privmsg :: String -> Net ()
privmsg s = do
    asks botChan >>= \c ->
        write "PRIVMSG" $ c ++ " :" ++ s

-- This writes a command and string to IRC. It also prints them to the screen.
write :: String -> String -> Net ()
write s t = do
    output <- return . botOutput =<< get
    io . output $ s ++ " " ++ t

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
          logMsg  = if logFlag then "on" else "off"
-- * `!echo NAME` — echo back;
eval (Message _ _ _ x) | "!echo" `L.isPrefixOf` x =
    privmsg (drop 6 x)
-- * `!search TERMS` — This searches the log and prints out the last 25
--   results;
eval (Message _ _ _ x) | "!search" `L.isPrefixOf` x = do
    db <- gets botDbHandle
    results <- io $ search db query
    mapM_ privmsg . map showSearchResult $ results
    privmsg $ show (length results) ++ " message(s) found."
    where query = drop 8 x
-- * `!mimic USER` — This outputs ten tokens that mimic USER according to a
-- very simple probabilistic model.
eval (Message _ _ _ x) | "!mimic" `L.isPrefixOf` x = do
    db       <- gets botDbHandle
    messages <- io $ return . map snd =<< getUserMessages db user
    tokens   <- io $ mimic messages 12
    privmsg $ L.intercalate " " tokens
    where user = drop 7 x
-- * All other `!` commands are ignored; and
eval (Message _ _ _ ('!':_)) = return ()
-- * All other messages are handled by `processMessage`.
eval msg = processMessage msg

-- This gets how long the bot has been running and returns it as a string.
uptime :: Net String
uptime = do
    now  <- io getCurrentTime
    zero <- return . botStartTime =<< get
    return . show $ diffUTCTime now zero

-- This processes an incoming message.
--
-- Currently, this means that we log it to the database and add its tokens to
-- the inverted index.
processMessage :: Message -> Net ()
processMessage msg = do
    db <- gets botDbHandle
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

