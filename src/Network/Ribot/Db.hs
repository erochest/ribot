
-- This contains the functions to interface with the database and to access it.
--
-- Ribot uses the database to log messages and it implements an inverted index
-- on it. (I could probably use SQLite's own extension for this, but it seemed
-- marginally more interesting to roll my own. I thought that there may be
-- problems making sure the extension is available on the thousands of machines
-- I expect to run Ribot on.)

module Network.Ribot.Db
    ( connectDb
    , IConnection(..)
    , ConnWrapper
    , logMessage
    , setUserLogging
    ) where

import           Control.Monad (forM, forM_, mapM, mapM_)
import           Database.HDBC
import           Database.HDBC.Types (IConnection(..), ConnWrapper)
import           Database.HDBC.Sqlite3 (connectSqlite3)
import           Network.Ribot.Message
import           System.Directory
import           System.FilePath ((</>))


-- This connects to the database. It returns a ConnWrapper in order to provide
-- some flexibility in what database engine I use.
connectDb :: IO ConnWrapper
connectDb =   findDbFile
          >>= connectSqlite3
          >>= initDb
          >>= createDb
          >>= return . ConnWrapper

-- This initializes the database by turning on appropriate pragmas.
initDb :: IConnection c => c -> IO c
initDb cxn = do
    runRaw cxn "PRAGMA foreign_keys = ON;"
    return cxn

-- This creates the database schema and returns the database connection so it
-- can be used for piping.
--
-- (I'm almost certain there's a one-liner for the main definition of this
-- function, but it's eluding me. I'll have to re-visit this once I have more
-- Haskell-fu.)
createDb :: IConnection c => c -> IO c
createDb =
    (flip withTransaction) $ \cxn -> do
        forM_ sql $ (runRaw cxn)
        return cxn
    where
        -- Here are the statements, packaged up so run by create.
        sql = [ "CREATE TABLE IF NOT EXISTS user ( \
                        \ id INTEGER PRIMARY KEY, \
                        \ username TEXT, \
                        \ logging_on BOOL \
                        \ );"
              , "CREATE TABLE IF NOT EXISTS message ( \
                        \ id INTEGER PRIMARY KEY, \
                        \ user_id INTEGER, \
                        \ text TEXT, \
                        \ posted DATETIME, \
                        \ FOREIGN KEY (user_id) REFERENCES user(id) \
                        \ );"
              , "CREATE TABLE IF NOT EXISTS token ( \
                        \ id INTEGER PRIMARY KEY, \
                        \ text TEXT \
                        \ );"
              , "CREATE TABLE IF NOT EXISTS position ( \
                        \ id INTEGER PRIMARY KEY, \
                        \ token_id INTEGER, \
                        \ message_id INTEGER, \
                        \ offset INTEGER, \
                        \ length INTEGER, \
                        \ FOREIGN KEY (token_id) REFERENCES token(id), \
                        \ FOREIGN KEY (message_id) REFERENCES message(id) \
                        \ );"
              , "CREATE UNIQUE INDEX IF NOT EXISTS idx_user ON user \
                        \ (username);"
              , "CREATE INDEX IF NOT EXISTS idx_message ON message \
                        \ (id, user_id, posted);"
              , "CREATE INDEX IF NOT EXISTS idx_token ON token (id, text);"
              , "CREATE INDEX IF NOT EXISTS idx_position ON position \
                        \ (id, token_id, message_id);"
              ]

-- This finds the database filename. It looks in the user application
-- directory.
findDbFile :: IO FilePath
findDbFile = do
    appDir <- getAppUserDataDirectory "ribot"
    createDirectoryIfMissing True appDir
    return $ appDir </> "ribot.db"

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
logMessage :: IConnection c => Message -> c -> IO ()
logMessage msg cxn = do
    -- If there's no user, move on.
    case (msgUser msg) of
        Just userName -> addMsg userName $ msgText msg
        Nothing -> return ()

    where
        addMsg :: String -> String -> IO ()
        addMsg userName msgStr = do
            -- First, try to create the user, if the name isn't already in the db.
            checkUser cxn userName
            -- Second, add the message to the database.
            run cxn "INSERT INTO message (user_id, text, posted) \
                \ SELECT id, DATETIME('NOW'), ? \
                \ FROM user WHERE username=? AND logging_on=1;"
                [toSql msgStr, toSql userName]

            return ()

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

