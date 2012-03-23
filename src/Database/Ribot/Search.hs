{-# LANGUAGE OverloadedStrings #-}

-- This handles searching the database. It returns the hits of messages.

module Database.Ribot.Search
    ( search
    , replaceWildCard
    , SearchResult(..)
    ) where

import           Control.Applicative ((<$>))
import           Data.Maybe (isJust)
import           Data.Monoid (mempty, Monoid)
import qualified Data.List as L
import qualified Data.Text as T
import           Data.Time.Format (formatTime)
import           Database.Persist
import qualified Database.Persist.GenericSql.Internal as I
import           Database.Persist.GenericSql.Raw (execute, getStmt, withStmt)
import           Database.Persist.Sqlite
import           Database.Persist.Store
import           Database.Ribot hiding (tokenText)
import           System.Locale (defaultTimeLocale)
import           Text.Bakers12.Tokenizer.Types (Token(..))
import           Text.Ribot.Tokenizer (tokenizeQuery)

-- This is the basic search result.
type UserEntity    = Entity User
type MessageEntity = Entity Message
data SearchResult  = SearchResult UserEntity MessageEntity


-- This returns the empty monoid value for Left Either values.
onlyRight :: Monoid b => Either a b -> b
onlyRight (Left _)    = mempty
onlyRight (Right val) = val


search :: FilePath -> Int -> String -> IO [SearchResult]
search dbFile searchMax queryString = withSqliteConn (T.pack dbFile) $ runSqlConn $ do
    map (uncurry SearchResult) <$> rawSql query params
    where queryTerms = map (T.map replaceWildCard . tokenText)
                     . onlyRight
                     . tokenizeQuery ""
                     . T.pack
                     $ queryString
          (query, params) = buildQuery queryTerms searchMax

-- This replaces the wildcard characters to SQL wildcards. Other characters
-- remain untouched.
replaceWildCard :: Char -> Char
replaceWildCard '*' = '%'
replaceWildCard c   = c

-- This takes a list of query terms and builds the query.
buildQuery :: [T.Text] -> Int -> (T.Text, [PersistValue])
buildQuery terms limit =
    (T.concat . L.reverse . (suffix:) $ wheres ++ sqlList, L.reverse params)
    where
        foldInit = ([prefix], [], [])

        (sqlList, wheres, params) =
            L.foldl' foldTerm foldInit . zip [0..] $ terms

        prefix = " SELECT DISTINCT ??, ?? \
                 \ FROM \"Message\" \
                 \ JOIN \"User\" on \"User\".id=\"Message\".userId "
        suffix = T.concat [ " ORDER BY \"Message\".posted DESC"
                          , " LIMIT ", T.pack $ show limit
                          ]

        join :: Int -> T.Text
        join n = T.concat [ " JOIN \"Position\" p", n'
                          , " ON p", n', ".messageId=\"Message\".id"
                          , " JOIN \"Token\" t", n'
                          , " ON t", n', ".id=p", n', ".tokenId "
                          ]
            where n' = T.pack $ show n

        where_ :: Int -> T.Text -> T.Text
        where_ n term = T.concat [ whereIntro n
                                 , " t", T.pack (show n), ".text"
                                 , whereOp term
                                 , "?"
                                 ]

        whereIntro :: Int -> T.Text
        whereIntro 0 = " WHERE "
        whereIntro _ = " AND "

        whereOp :: T.Text -> T.Text
        whereOp term | isJust $ T.find isWild term = " LIKE "
                     | otherwise                   = "="

        isWild :: Char -> Bool
        isWild '%' = True
        isWild _   = False

        foldTerm :: ([T.Text], [T.Text], [PersistValue])
                 -> (Int, T.Text)
                 -> ([T.Text], [T.Text], [PersistValue])
        foldTerm (sql, wheres', params') (n, term) =
            (join n : sql, where_ n term : wheres', toPersistValue term : params')

instance Show SearchResult where

    show (SearchResult (Entity _ user) (Entity _ message)) =
        T.unpack $ T.concat [ userUsername user
                            , " at "
                            , T.pack . formatTime defaultTimeLocale "%c" $ messagePosted message
                            , ": "
                            , messageText message
                            ]


