
-- This has functions for setting up and running the IRC client.

module Network.Ribot.Irc
    ( initParts
    , runConsole
    , runDaemon
    ) where

import           Control.Concurrent
import           Control.Monad (forever)
import           Control.Monad.IO.Class (liftIO)
import           Data.Ribot.Config
import           Data.Configurator.Types (Config)
import           Data.Time
import           Network.IRC.Bot
import           Network.IRC.Bot.Part.Channels
import           Network.IRC.Bot.Part.Dice
import           Network.IRC.Bot.Part.Hello
import           Network.IRC.Bot.Part.NickUser
import           Network.IRC.Bot.Part.Ping
import           Network.Ribot.Irc.Part.Echo (echoPart)
import           Network.Ribot.Irc.Part.Knitter (knitterPart)
import           Network.Ribot.Irc.Part.LogToggle (logTogglePart)
import           Network.Ribot.Irc.Part.Mimic (mimicPart)
import           Network.Ribot.Irc.Part.Search (searchPart)
import           Network.Ribot.Irc.Part.Topic (topicPart)
import           Network.Ribot.Irc.Part.UpTime (uptimePart)
import           Network.Ribot.Irc.Part.Version (versionPart)

-- This initializes the parts (plugins) for the Ribot bot.
initParts :: (BotMonad m) => BotConf -> Config -> IO [m ()]
initParts config ribotConfig = do
    dbFile        <- ribotDbFile ribotConfig
    pbKey         <- ribotPasteBin ribotConfig
    searchMax     <- ribotSearchMax ribotConfig
    (_, chanPart) <- initChannelsPart $ channels config
    now <- liftIO getCurrentTime
    return [ pingPart
           , nickUserPart
           , chanPart
           , dicePart
           , helloPart
           , echoPart
           , logTogglePart dbFile
           , mimicPart dbFile
           , searchPart dbFile pbKey searchMax
           , topicPart dbFile pbKey searchMax
           , uptimePart now
           , versionPart
           , knitterPart dbFile
           ]

-- This runs the bot on the console.
runConsole :: BotConf -> Config -> IO ()
runConsole config ribotConfig = do
    parts <- initParts config ribotConfig
    tids  <- simpleBot config parts
    (logger config) Important "Press ENTER to quit."
    getLine
    mapM_ killThread tids

-- This runs the bot in daemon mode.
runDaemon :: BotConf -> Config -> IO ()
runDaemon config ribotConfig =   initParts config ribotConfig
                             >>= simpleBot config
                             >>  forever (threadDelay 100000)

