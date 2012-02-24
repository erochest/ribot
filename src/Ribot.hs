
-- This is the entry point for Ribot.

module Main where

import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.Chan
import qualified Data.Set as S
import           Network
import           Network.BSD
import           Network.IRC.Base
import           Network.IRC.Bot
import           Network.IRC.Bot.Log
import           Network.IRC.Bot.Part.Channels
import           Network.IRC.Bot.Part.Dice
import           Network.IRC.Bot.Part.Hello
import           Network.IRC.Bot.Part.NickUser
import           Network.IRC.Bot.Part.Ping

ribotServer :: HostName
ribotServer = "irc.freenode.org"

ribotPort :: PortID
ribotPort = PortNumber 6667

ribotChan :: S.Set String
ribotChan = S.singleton "#err1234567890"

ribotNick :: String
ribotNick = "ribtest"

main :: IO ()
main = do
    config <- initConfig
    parts  <- initParts
    tids   <- simpleBot config parts
    (logger config) Important "Press ENTER to quit."
    getLine
    mapM_ killThread tids

initParts :: (BotMonad m) => IO [m ()]
initParts = do
    (_, chanParts) <- initChannelsPart ribotChan
    return [ pingPart
           , nickUserPart
           , chanParts
           , dicePart
           , helloPart
           ]

initConfig :: IO BotConf
initConfig = do
    host <- getHostName
    return (BotConf
        (Just writeMsg)
        (stdoutLogger Debug)
        ribotServer
        ribotPort
        ribotNick
        "!"
        (User ribotNick host ribotServer ribotNick)
        ribotChan
        )

writeMsg :: Chan Message -> IO ()
writeMsg chan = readChan chan >>= putStrLn . ("MESSAGE: " ++) . show

