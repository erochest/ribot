
module Network.Ribot.Core
    ( Ribot (..)
    , Net
    , connect
    , runRibot
    , write
    , clean
    , privmsg
    ) where

import           Control.Exception (bracket_)
import           Control.Monad.Reader
import qualified Data.List as L
import           Data.Time
import           Network
import           System.Exit
import           System.IO
import           Text.Printf

data Ribot = Ribot { botSocket    :: Handle
                   , botServer    :: String
                   , botPort      :: Int
                   , botChan      :: String
                   , botNick      :: String
                   , botStartTime :: UTCTime
                   }

type Net = ReaderT Ribot IO

connect :: String -> Int -> String -> String -> IO Ribot
connect server port chan nick = notify $ do
    h <- connectTo server . PortNumber $ fromIntegral port
    t <- getCurrentTime
    hSetBuffering h NoBuffering
    return $ Ribot h server port chan nick t
    where
        notify a = bracket_
            (printf "Connecting to %s ..." server >> hFlush stdout)
            (putStrLn "done.")
            a

-- Run in Net
runRibot :: Net ()
runRibot = do
    n <- asks botNick
    write "NICK" n
    write "USER" (n ++ " 0 * :riBOT")
    asks botChan >>= write "JOIN"
    asks botSocket >>= listen

io :: IO a -> Net a
io = liftIO

write :: String -> String -> Net ()
write s t = do
    h <- asks botSocket
    io $ hPrintf h "%s %s\r\n" s t
    io $ printf    "> %s %s\n" s t

listen :: Handle -> Net ()
listen h = forever $ do
    s <- init `fmap` io (hGetLine h)
    io $ putStrLn s
    if ping s then pong s else eval (clean s)
    where
        forever a = a >> forever a
        ping x = "PING:" `L.isPrefixOf` x
        pong x = write "PONG" (':' : drop 6 x)

clean :: String -> String
clean = drop 1 . dropWhile (/= ':') . drop 1

eval :: String -> Net ()
eval "!quit"                    = write "QUIT" ":Exiting" >> io (exitWith ExitSuccess)
eval "!uptime"                  = uptime >>= privmsg
eval x | "!id" `L.isPrefixOf` x = privmsg (drop 4 x)
eval _                          = return ()

uptime :: Net String
uptime = do
    now <- io getCurrentTime
    zero <- asks botStartTime
    return . show $ diffUTCTime now zero

privmsg :: String -> Net ()
privmsg s = do
    asks botChan >>= \c ->
        write "PRIVMSG" $ c ++ " :" ++ s

