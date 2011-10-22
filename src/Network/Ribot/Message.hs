
-- These are functions and data types related to messages.

module Network.Ribot.Message
    ( Message(..)
    , parseMessage
    ) where

import           Data.Time
import           Text.ParserCombinators.Parsec

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


