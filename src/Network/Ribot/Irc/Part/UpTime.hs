
-- This handles the `!uptime` command. This gives the difference in the current
-- time and the time the part was started, which is assumed to be the bot's
-- start time.

module Network.Ribot.Irc.Part.UpTime
    ( uptimePart
    , uptimeCommand
    ) where


import           Control.Monad.IO.Class (liftIO, MonadIO)
import qualified Data.List as L
import           Data.Time
import           Network.IRC.Bot.BotMonad (BotMonad(..), maybeZero)
import           Network.IRC.Bot.Commands (PrivMsg(..), askSenderNickName,
                                           replyTo, sendCommand)
import           Network.IRC.Bot.Log (LogLevel(Debug))
import           Network.IRC.Bot.Parsec (botPrefix, parsecPart)
import           Text.Parsec (ParsecT, (<|>), string, try)

-- This integrates this part into Ribot.
uptimePart :: BotMonad m => UTCTime -> m ()
uptimePart = parsecPart . uptimeCommand

-- This parses and handles the `!uptime` command.
uptimeCommand :: BotMonad m => UTCTime -> ParsecT String () m ()
uptimeCommand start = uptime <|> return ()
    where
        uptime :: BotMonad m => ParsecT String () m ()
        uptime = try $ do
            botPrefix
            string "uptime"
            target <- maybeZero =<< replyTo
            nick   <- maybeZero =<< askSenderNickName
            now    <- liftIO getCurrentTime
            let elapsed = diffUTCTime now start
                display = showElapsed $ round elapsed
                reply   = "I've been up " ++ display
            logM Debug $ "!uptime = " ++ show elapsed ++  " => "  ++ display
            sendCommand . PrivMsg Nothing [target] $ nick ++ ", " ++ reply

-- This takes the elapsed number of seconds as an Integral and returns a
-- human-readable string listing the elapsed time.
showElapsed :: Int -> String
showElapsed = L.intercalate ", " . L.reverse . show' units
    where
        units :: [(Int, String)]
        units = [ (60, "seconds")
                , (60, "minutes")
                , (60, "hours")
                , (24, "days")
                , (7, "weeks")
                ]

        show' :: [(Int, String)] -> Int -> [String]
        show' [] _ = []
        show' _  0 = []
        show' ((n, units'):us) elapsed | m == 0    = show' us d
                                       | otherwise = m' : show' us d
                where (d, m) = elapsed `divMod` n
                      m'     = show m ++ " " ++ units'

