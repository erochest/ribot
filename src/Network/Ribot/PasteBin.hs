
-- This is an interface over http://pastebin.com/api.

module Network.Ribot.PasteBin
    ( PasteBinApiKey
    , createPaste
    , bulkPrivMsg
    ) where

import           Control.Monad.IO.Class (liftIO)
import qualified Data.List as L
import           Data.Maybe
import           Network.HTTP
import           Network.IRC.Bot.BotMonad (BotMonad(..))
import           Network.IRC.Bot.Commands (PrivMsg(..), sendCommand)
import           Network.URI (nullURI, parseURI, URI(..))

-- This is for the PasteBin API key.
type PasteBinApiKey = String

-- This is the base API URL.
pasteUri :: URI
pasteUri = fromMaybe nullURI $ parseURI "http://pastebin.com/api/api_post.php"

-- Given the developer key and some content, this creates a new paste. It
-- returns the URL for the paste.
createPaste :: PasteBinApiKey -> String -> String -> IO String
createPaste apiKey title content =
    getResponseBody =<< simpleHTTP req
    where
        fields  = [ ("api_option", "paste")
                  , ("api_paste_private", "0")
                  , ("api_paste_name", title)
                  , ("api_paste_expire_date", "1H")
                  , ("api_dev_key", apiKey)
                  , ("api_paste_code", content)
                  ]
        query   = urlEncodeVars fields
        headers = [ mkHeader HdrContentType "application/x-www-form-urlencoded"
                  , mkHeader HdrContentLength . show $ length query
                  ]
        req     = Request { rqURI     = pasteUri
                          , rqMethod  = POST
                          , rqHeaders = headers
                          , rqBody    = query
                          }

-- This is a send command that wraps `sendCommand` and `PrivMsg` with one
-- receiver.
send :: BotMonad m => String -> String -> m ()
send receiver = sendCommand . PrivMsg Nothing [receiver]

-- This takes a header message and a list of other messages. If there are more
-- than 3 total messages and there's a PasteBin key, they're all posted to
-- PasteBin and the header message and a link to the PasteBin are posted to
-- IRC.
bulkPrivMsg :: BotMonad m
            => Maybe PasteBinApiKey
            -> String
            -> String
            -> [String]
            -> m ()
bulkPrivMsg _ receiver header msgs | length msgs <= 3 =
    mapM_ (send receiver) (header:msgs)
bulkPrivMsg Nothing receiver header msgs = do
    send receiver header
    mapM_ (send receiver) $ take 2 msgs
    send receiver "(I didn't send everything, 'cause I don't want to get booted.)"
bulkPrivMsg (Just key) receiver header msgs = do
    url <- liftIO $ createPaste key header $ L.unlines msgs
    send receiver header
    send receiver "That's too many for me to post here."
    send receiver $ "So they'll be available at " ++ url ++ " for about an hour."

