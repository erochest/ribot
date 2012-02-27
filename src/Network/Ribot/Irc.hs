
-- This has functions for setting up and running the IRC client.

module Network.Ribot.Irc
    ( initParts
    , runConsole
    , runDaemon
    ) where

import           Control.Concurrent
import           Control.Monad (forever)
import           Network.IRC.Bot
import           Network.IRC.Bot.Part.Channels
import           Network.IRC.Bot.Part.Dice
import           Network.IRC.Bot.Part.Hello
import           Network.IRC.Bot.Part.NickUser
import           Network.IRC.Bot.Part.Ping

-- This initializes the parts (plugins) for the Ribot bot.
initParts :: (BotMonad m) => BotConf -> IO [m ()]
initParts config = do
    (_, chanPart) <- initChannelsPart $ channels config
    return [ pingPart
           , nickUserPart
           , chanPart
           , dicePart
           , helloPart
           ]

-- This runs the bot on the console.
runConsole :: BotConf -> IO ()
runConsole config = do
    parts <- initParts config
    tids  <- simpleBot config parts
    (logger config) Important "Press ENTER to quit."
    getLine
    mapM_ killThread tids

-- This runs the bot in daemon mode.
runDaemon :: BotConf -> IO ()
runDaemon config =   initParts config
                 >>= simpleBot config
                 >>  forever (threadDelay 100000)

