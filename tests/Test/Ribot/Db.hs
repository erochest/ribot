{-# LANGUAGE FlexibleContexts #-}

-- This contains functions and utilities for working with the database in
-- tests.

module Test.Ribot.Db
    ( withTempDb
    , getScalar
    , lastInsertRowId
    , insertUser
    , insertMsg
    ) where

import           Control.Exception (bracket)
import           Data.Convertible (Convertible)
import qualified Data.List as L
import           Database.HDBC
import           Database.HDBC.Sqlite3 (connectSqlite3, Connection)
import           Database.Ribot (createDb, initDb)
import           Test.HUnit (Assertion)

-- This creates a temporary database with the schema for ribot and runs the
-- test `Assertion` in it.
withTempDb :: (Connection -> Assertion) -> Assertion
withTempDb = bracket (connectSqlite3 ":memory:" >>= initDb >>= createDb)
                     disconnect

-- This picks the scalar result from the returned query list.
getScalar :: Convertible SqlValue a => [[SqlValue]] -> a
getScalar = fromSql . L.head . L.head

-- This gets the `LAST_INSERT_ROWID()`.
lastInsertRowId :: IConnection c => c -> IO Int
lastInsertRowId cxn =
    quickQuery' cxn "SELECT LAST_INSERT_ROWID();" [] >>=
    return . getScalar

-- Insert a user into the database. Its returns the user's ID.
insertUser :: IConnection c => c -> String -> IO Int
insertUser cxn nick =
    run cxn
        "INSERT INTO user (username, logging_on) \
        \ VALUES (?, 1);"
        [toSql nick] >>
    lastInsertRowId cxn

-- Insert a message into the database. It returns the message's ID.
insertMsg :: IConnection c => c -> Int -> String -> IO Int
insertMsg cxn uId m =
    run cxn
        "INSERT INTO message (user_id, text, posted) \
        \ VALUES (?, ?, DATETIME('NOW'));"
        [toSql uId, toSql m] >>
    lastInsertRowId cxn

