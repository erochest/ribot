
-- |
-- Module      : Text.Bakers12.Tokenizer.Types
-- License     : Apache 2.0
-- Maintainer  : erochest@gmail.com
-- Portability : GHC
--
-- This defines the types for the tokenizers. Especially, this defines the
-- Token and TokenType data types.

module Text.Bakers12.Tokenizer.Types
    ( Token(..)
    , TokenType(..)
    , empty
    , isReadable
    , append
    , concat
    , splitAt
    ) where

import qualified Data.List as L
import qualified Data.Text as T
import           Prelude hiding (concat, splitAt)

-- * Token Type

-- | This contains data from one token instance.
data Token = Token
    { tokenText   :: T.Text         -- ^ The normalized token text.
    , tokenRaw    :: T.Text         -- ^ The raw token text.
    , tokenLength :: Int            -- ^ The length of the raw token.
    , tokenType   :: TokenType      -- ^ The type of data contained in the
                                    -- token.
    , tokenSource :: FilePath       -- ^ The source (sometimes a file) that
                                    -- the token was found in.
    , tokenOffset :: Integer        -- ^ The character offset of the start of
                                    -- the token in the source (zero-indexed).
    }
    deriving (Eq, Show)

-- | This has the types of information that a token can contain.
data TokenType =
      AlphaToken                    -- ^ Unicode alphabetic characters.
    | NumberToken                   -- ^ Unicode numeric characters.
    | SeparatorToken                -- ^ Unicode space or Unicode separator
                                    -- character.
    | PunctuationToken              -- ^ One Unicode punctuation character.
    | SymbolToken                   -- ^ One Unicode symbol character.
    | MarkToken                     -- ^ One Unicode mark character.
    | UnknownToken                  -- ^ None of the categories above.
    deriving (Eq, Show)

-- | This is an empty token.
empty :: Token
empty = Token T.empty T.empty 0 UnknownToken "" 0

-- | This tests whether a token is "readable." I.e., whether its type is
-- AlphaToken or NumberToken.
isReadable :: Token -> Bool
isReadable token = tType == AlphaToken || tType == NumberToken
    where tType = tokenType token

-- | This takes two tokens (presumably right next to each other in the input
-- stream, although this doesn't verify that) and concatenates them by
-- concatenating their text and raw data and their lengths.
append :: Token -> Token -> Token
append a b =
    a { tokenText   = T.append (tokenText a) (tokenText b)
      , tokenRaw    = T.append (tokenRaw a) (tokenRaw b)
      , tokenLength = tokenLength a + tokenLength b
      }

-- | This takes a list of tokens and appends them. Like append above, it
-- assumes that the tokens occur right next to each other.
--
-- The implementation of this could use fold, but I thought that using T.concat
-- and not constructing any intermediate Tokens would be faster.
concat :: [Token] -> Token
concat []       = empty
concat [t]      = t
concat [t1, t2] = append t1 t2
concat ts@(a:_) =
    a { tokenText   = T.concat texts
      , tokenRaw    = T.concat raws
      , tokenLength = sum lengths
      }
    where
        -- Another small optimization, so we don't have to walk the input list
        -- more than once. If these operations can be fused, that would be
        -- best, but even just walking the list twice is better than walking it
        -- three times. (Best would be to write unzipWith3 or something, which
        -- would insure that the list was only walked twice. Someday.)
        (texts, raws, lengths) = L.unzip3 . map getFields $ ts
        getFields t = (tokenText t, tokenRaw t, tokenLength t)

-- | This splits a token on position N in the tokenText. This copies the raw
-- token, unchanged, into each of the resulting tokens. However, the offset of
-- the second token is moved up and the length of the tokens reflects the
-- length of the split token texts.
splitAt :: Int -> Token -> (Token, Token)
splitAt n token = (t1, t2)
    where (text1, text2) = T.splitAt n $ tokenText token
          t1 = token { tokenText   = text1
                     , tokenLength = T.length text1
                     }
          t2 = token { tokenText   = text2
                     , tokenOffset = tokenOffset token + fromIntegral (T.length text1)
                     , tokenLength = T.length text2
                     }

