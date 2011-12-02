
-- This is an interface over http://pastebin.com/api.

module Network.Ribot.PasteBin
    ( createPaste
    ) where

import           Data.Maybe
import           Network.HTTP
import           Network.URI (nullURI, parseURI, URI(..))

-- This is the base API URL.
pasteUri :: URI
pasteUri = fromMaybe nullURI $ parseURI "http://pastebin.com/api/api_post.php"

-- Given the developer key and some content, this creates a new paste. It
-- returns the URL for the paste.
createPaste :: String -> String -> String -> IO String
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

