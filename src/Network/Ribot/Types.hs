
-- These are functions and data types related to messages.

module Network.Ribot.Types
    ( Ribot(..)
    , RibotState(..)
    , Net
    , runNet
    , NetState
    , runNetState
    , Message(..)
    , parseMessage
    ) where

import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Time
import           Database.HDBC (ConnWrapper)
import           System.IO
import           Text.ParserCombinators.Parsec

-- This is the main data structure for the bot. It has connection information,
-- connection handles for IRC and the database, and the time the bot started
-- (for `!uptime`).
data Ribot = Ribot { botSocket    :: Handle
                   , botServer    :: String
                   , botPort      :: Int
                   , botChan      :: String
                   , botNick      :: String
                   , botStartTime :: UTCTime
                   , botDbHandle  :: ConnWrapper
                   , botOutput    :: String -> IO ()
                   }

-- This adds a little state to the Ribot bot. Currently, that's just the time
-- of the last data sent from the server.
data RibotState = RibotState { botLastMessage :: UTCTime
                             }

-- This is the monad the bot runs under. The `ReaderT` holds the state (a
-- `Ribot`) and silently threads it through the computation.
type Net = ReaderT Ribot IO

-- This is the primary execution function for the `Net` monad.
runNet :: Net a -> Ribot -> IO a
runNet = runReaderT

-- This is the `Net` monad, plus `RibotState`.
type NetState = StateT RibotState Net

-- This is the primary execution function for the `NetState` monad.
runNetState :: NetState a -> Ribot -> RibotState -> IO a
runNetState ns r rs = runReaderT (evalStateT ns rs) r

-- This is a storage for incoming messages.
data Message = Message { msgUser :: Maybe String
                       , msgChan :: Maybe String
                       , msgTime :: UTCTime
                       , msgText :: String
                       }
    deriving (Show)

-- This parses an incoming string into a Message.
parseMessage :: String -> IO Message
parseMessage input =
    case (parseMsg input) of
        Right [user, chan, msg] -> do
            now <- getCurrentTime
            return $ Message (Just user) (Just chan) now msg
        Left _ -> do
            now <- getCurrentTime
            return $ Message Nothing Nothing now input
    where
        -- And example line would be:
        -- ":erochester!~erocheste@137.54.2.108 PRIVMSG #err1234567890 :this is a message"
        -- Which parses as something like this: `:USER!.*#CHAN :MSG`.
        parseMsg :: String -> Either ParseError [String]
        parseMsg = parse ircLine "(unknown)"

        -- This represents one line from the IRC server.
        ircLine = do
            char ':'
            user <- ircUser
            skipHost
            chan <- ircChan
            spaces
            char ':'
            msg <- ircMsg
            return [user, '#':chan, msg]
        -- An IRC username is everything up to the `!`.
        ircUser = many (noneOf "!")
        -- An IRC channel is everything from `#` up to a `#` or `!`.
        ircChan = do
            char '#'
            many (noneOf " !#")
        -- An IRC message is everything until the end of the string.
        ircMsg = many anyChar
        -- This skeps everything to `#`.
        skipHost = skipMany (noneOf "#")


