
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


connectDb :: IO ConnWrapper
connectDb = findDbFile >>= connectSqlite3 >>= return . ConnWrapper

findDbFile :: IO FilePath
findDbFile = do
    appDir <- getAppUserDataDirectory "ribot"
    createDirectoryIfMissing True appDir
    return $ appDir </> "ribot.db"

