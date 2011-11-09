
-- This contains the functions to interface with the database and to access it.
--
-- Ribot uses the database to log messages and it implements an inverted index
-- on it. (I could probably use SQLite's own extension for this, but it seemed
-- marginally more interesting to roll my own. I thought that there may be
-- problems making sure the extension is available on the thousands of machines
-- I expect to run Ribot on.)

module Database.Ribot
    ( connectDb
    , IConnection(..)
    , ConnWrapper
    , createDb
    , initDb
    , initTempTable
    ) where

import           Control.Monad (forM, forM_, mapM, mapM_)
import           Database.HDBC
import           Database.HDBC.Types (IConnection(..), ConnWrapper)
import           Database.HDBC.Sqlite3 (connectSqlite3)
import           Network.Ribot.Types
import           System.Directory
import           System.FilePath ((</>))


-- This connects to the database. It returns a ConnWrapper in order to provide
-- some flexibility in what database engine I use.
connectDb :: Maybe String -> IO ConnWrapper
connectDb dbFile = getDbFile
          >>= connectSqlite3
          >>= initDb
          >>= createDb
          >>= return . ConnWrapper
    where getDbFile = case dbFile of
                        Nothing  -> findDbFile
                        Just ""  -> findDbFile
                        Just dbf -> return dbf

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
        initTempTable cxn
        return cxn
    where
        -- Here are the statements, packaged up so run by create.
        --
        -- `user` has an index of the nicks seen on the channels this bot has
        -- been on.
        sql = [ "CREATE TABLE IF NOT EXISTS user ( \
                        \ id INTEGER PRIMARY KEY, \
                        \ username TEXT, \
                        \ logging_on BOOL \
                        \ );"
              -- `message` are the messages and dates, linked to the nicks that
              -- sent them.
              , "CREATE TABLE IF NOT EXISTS message ( \
                        \ id INTEGER PRIMARY KEY, \
                        \ user_id INTEGER, \
                        \ text TEXT, \
                        \ posted DATETIME, \
                        \ FOREIGN KEY (user_id) REFERENCES user(id) \
                        \ );"
              -- `token` is an index of the token types in the inverted index.
              , "CREATE TABLE IF NOT EXISTS token ( \
                        \ id INTEGER PRIMARY KEY, \
                        \ text TEXT UNIQUE ON CONFLICT IGNORE \
                        \ );"
              -- `position` links the token types seen (in `token`) to the
              -- messages they were seen in. At some point in the future, this
              -- may include the token's positions in the message, but it
              -- doesn't now.
              , "CREATE TABLE IF NOT EXISTS position ( \
                        \ id INTEGER PRIMARY KEY, \
                        \ token_id INTEGER, \
                        \ message_id INTEGER, \
                        \ FOREIGN KEY (token_id) REFERENCES token(id), \
                        \ FOREIGN KEY (message_id) REFERENCES message(id) \
                        \ );"
              -- `idx_user` makes sure that the nicks are unique.
              , "CREATE UNIQUE INDEX IF NOT EXISTS idx_user ON user \
                        \ (username);"
              -- `idx_message` indexes messages by user and date posted.
              , "CREATE INDEX IF NOT EXISTS idx_message ON message \
                        \ (id, user_id, posted);"
              -- `idx_token` indexes tokens on text.
              , "CREATE INDEX IF NOT EXISTS idx_token ON token (id, text);"
              -- `idx_position` indexes position by token and message.
              , "CREATE INDEX IF NOT EXISTS idx_position ON position \
                        \ (id, token_id, message_id);"
              ]

-- This initializes `msg_token`, the temporary scratch table for building the
-- inverted index.
initTempTable :: IConnection c => c -> IO ()
initTempTable cxn =
    runRaw cxn " CREATE TEMPORARY TABLE IF NOT EXISTS msg_token \
               \ (token_id INTEGER DEFAULT NULL, \
               \  message_id INTEGER, \
               \  text TEXT, \
               \  UNIQUE (message_id, text) ON CONFLICT IGNORE, \
               \  FOREIGN KEY (message_id) REFERENCES message(id) \
               \ );"

-- This finds the database filename. It looks in the user application
-- directory.
findDbFile :: IO FilePath
findDbFile = do
    appDir <- getAppUserDataDirectory "ribot"
    createDirectoryIfMissing True appDir
    return $ appDir </> "ribot.db"

