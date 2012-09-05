{-# LANGUAGE OverloadedStrings #-}

-- This is the entry point for Ribot.

module Main where

import           Control.Concurrent
import           Control.Monad (forM_)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Configurator as C
import           Data.Configurator.Types (Config)
import           Data.Ribot.Config
import qualified Data.Text as T
import           Database.Persist.Sqlite
import           Database.Ribot hiding (Message)
import qualified Database.Ribot as D
import           Database.Ribot.Index (indexItem, reindex)
import           Database.Ribot.Search (search)
import           Network.IRC.Base
import           Network.IRC.Bot
import           Network.Ribot.Irc
import           Ribot.Cli
import           System.Console.CmdArgs (cmdArgs)
import           System.Posix.Daemonize (daemonize)
import qualified Text.Ribot.Mimic as M


main :: IO ()
main = do
    mode <- cmdArgs ribotModes
    case config mode of
        Just fileName -> do
            config <- readConfig fileName
            dbFile <- ribotDbFile config
            handleMode mode config dbFile
        Nothing       -> putStrLn "You must specify a configuration file."

-- This handles dispatching to the modes.
handleMode :: Modes -> Config -> String -> IO ()
handleMode (Listen _) config dbFile = do
    config' <- createBotConf config $ writeMsg dbFile
    case config' of
        Just cfg -> runBot config cfg
        Nothing  -> putStrLn "You are missing configuration keys."
handleMode (Search _ terms) config dbFile = do
    searchMax <- ribotSearchMax config
    replies   <- search dbFile searchMax terms'
    putStrLn $ "SEARCH: " ++ terms'
    putStrLn $ (show (length replies)) ++ " results."
    forM_ replies (putStrLn . show)
    where terms' = unwords terms
handleMode (Ribot.Cli.Topic _ terms) config dbFile = do
    return ()
handleMode (Reindex _) config dbFile = do
    runPool dbFile 4 reindex
    putStrLn "done"
handleMode (Mimic _ userName) config dbFile =
    runDb dbFile $ do
        msgs'  <- getUserMessages userName'
        output <- case msgs' of
            Nothing -> return $ T.concat [ "I haven't heard "
                                         , userName'
                                         , " say anthying."
                                         ]
            Just [] -> return $ T.concat [ userName'
                                         , " hasn't said anything."
                                         ]
            Just msgs -> do
                let messages = map messageObj msgs
                liftIO $ T.unwords `fmap` M.mimic messages
        liftIO . putStrLn $ T.unpack output
    where
        userName' = T.pack userName
        messageObj :: Entity D.Message -> D.Message
        messageObj (Entity _ m) = m

-- This initializes the database and runs the bot either as a daemon or a
-- console program.
runBot :: Config -> BotConf -> IO ()
runBot cfg botConfig = do
    dbFile <- ribotDbFile cfg
    initDatabase dbFile
    asDaemon <- ribotDaemonize cfg
    if asDaemon
        then daemonize $ runDaemon botConfig cfg
        else runConsole botConfig cfg

-- This processes the messages by logging and indexing them.
writeMsg :: FilePath -> Chan Message -> IO ()
writeMsg dbFile chan = runPool dbFile 4 $
    (liftIO $ getChanContents chan) >>= mapM_ process
    where
        process msg = saveMessage msg >>= indexItem

