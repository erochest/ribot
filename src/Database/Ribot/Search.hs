{-# LANGUAGE OverloadedStrings #-}

-- This handles searching the database. It returns the hits of messages.

module Database.Ribot.Search
    ( search
    , topic
    , replaceWildCard
    , SearchResult(..)
    , TopicResult(..)
    ) where

import           Control.Applicative ((<$>))
import           Data.Maybe (isJust)
import           Data.Monoid (mempty, Monoid)
import qualified Data.List as L
import qualified Data.Text as T
import           Data.Time.Format (formatTime)
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.Store
import           Database.Ribot hiding (tokenText)
import           System.Locale (defaultTimeLocale)
import           Text.Bakers12.Tokenizer.Types (Token(..))
import           Text.Ribot.Tokenizer (tokenizeQuery)

-- This is the basic search result.
type UserEntity    = Entity User
type MessageEntity = Entity Message
type TopicEntity   = Entity Topic
data SearchResult  = SearchResult UserEntity MessageEntity
data TopicResult   = TopicResult UserEntity TopicEntity


-- This returns the empty monoid value for Left Either values.
onlyRight :: Monoid b => Either a b -> b
onlyRight (Left _)    = mempty
onlyRight (Right val) = val


search :: FilePath -> Int -> String -> IO [SearchResult]
search dbFile searchMax queryString = withSqliteConn (T.pack dbFile) $ runSqlConn $
    map (uncurry SearchResult) <$> rawSql query params
    where queryTerms = map (T.map replaceWildCard . tokenText)
                     . onlyRight
                     . tokenizeQuery ""
                     . T.pack
                     $ queryString
          (query, params) = buildQuery "Message" queryTerms searchMax

topic :: FilePath -> Int -> String -> IO [TopicResult]
topic dbFile searchMax queryString = withSqliteConn (T.pack dbFile) $ runSqlConn $
    map (uncurry TopicResult) <$> rawSql query params
    where queryTerms = map (T.map replaceWildCard . tokenText)
                     . onlyRight
                     . tokenizeQuery ""
                     . T.pack
                     $ queryString
          (query, params) = buildQuery "Topic" queryTerms searchMax

-- This replaces the wildcard characters to SQL wildcards. Other characters
-- remain untouched.
replaceWildCard :: Char -> Char
replaceWildCard '*' = '%'
replaceWildCard c   = c

-- This takes a list of query terms and builds the query.
buildQuery :: T.Text -> [T.Text] -> Int -> (T.Text, [PersistValue])
buildQuery target terms limit =
    (T.concat . L.reverse . (suffix:) $ wheres ++ sqlList, L.reverse params)
    where
        ltarget = T.toLower target

        foldInit = ([prefix], [], [])

        (sqlList, wheres, params) =
            L.foldl' foldTerm foldInit . zip [0..] $ terms

        prefix = T.concat [ " SELECT DISTINCT ??, ?? "
                          , " FROM \"", target, "\""
                          , " JOIN \"User\" on \"User\".id=\"", target, "\".userId "
                          ]
        suffix = T.concat [ " ORDER BY \"", target, "\".posted DESC"
                          , " LIMIT ", T.pack $ show limit
                          ]

        join :: Int -> T.Text
        join n = T.concat [ " JOIN \"Position\" p", n'
                          , " ON p", n', ".", ltarget, "Id=\"", target, "\".id"
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
                            , ": \""
                            , messageText message
                            , "\""
                            ]

instance Show TopicResult where

    show (TopicResult (Entity _ user) (Entity _ tpc)) =
        T.unpack $ T.concat [ userUsername user
                            , " at "
                            , T.pack . formatTime defaultTimeLocale "%c" $ topicPosted tpc
                            , ": \""
                            , topicText tpc
                            , "\""
                            ]


