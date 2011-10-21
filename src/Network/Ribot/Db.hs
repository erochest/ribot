
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
    ) where

import           Database.HDBC
import           Database.HDBC.Types (IConnection(..), ConnWrapper)
import           Database.HDBC.Sqlite3 (connectSqlite3)
import           System.Directory
import           System.FilePath ((</>))


-- This connects to the database. It returns a ConnWrapper in order to provide
-- some flexibility in what database engine I use.
connectDb :: IO ConnWrapper
connectDb = findDbFile >>= connectSqlite3 >>= return . ConnWrapper

-- This finds the database filename. It looks in the user application
-- directory.
findDbFile :: IO FilePath
findDbFile = do
    appDir <- getAppUserDataDirectory "ribot"
    createDirectoryIfMissing True appDir
    return $ appDir </> "ribot.db"

