
{-|
 - Module      : Text.Bakers12.Tokenizer
 - License     : Apache 2.0
 - Maintainer  : erochest@gmail.com
 - Portability : GHC
 - 
 - This implements the tokenizer. It returns all tokens, without throwing any
 - text away. That way, the unprocessed token stream can be used to re-create
 - the input.
 - 
 - The processing uses the
 - [enumerator](http://hackage.haskell.org/package/enumerator) library to
 - produce a stream of tokens from a stream of input.
 - 
 - The `tokenize` and `tokenizeFile` functions hide this by accumulating the
 - output tokens into a list and returning that.
 -}

module Text.Bakers12.Tokenizer
    ( Token(..)
    , TokenType(..)
    , tokenize
    , tokenizeFile
    , tokenizeFileStream
    , tokenizeStream
    , tokenizeE
    ) where

import           Control.Exception (SomeException)
import           Control.Monad (liftM)
import           Control.Monad.Trans (lift)
import qualified Data.Char as C
import qualified Data.Enumerator as E
import qualified Data.Enumerator.Binary as EB
import qualified Data.Enumerator.List as EL
import qualified Data.Enumerator.Text as ET
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import           Text.Bakers12.Tokenizer.Types

-- | This reads text from an instance of Data.Text.Text and returns a list of
-- Token instances.
tokenize :: FilePath -> T.Text -> Either SomeException [Token]
tokenize source input =
    E.runLists [[input]] (tokenizeStream source 0 E.=$ EL.consume)

-- | This reads the input from a file and returns a list of Token instances.
tokenizeFile :: FilePath -> IO (Either SomeException [Token])
tokenizeFile inputFile =
    E.run (tokenizeFileStream inputFile E.$$ EL.consume)

-- | This creates an Enumerator that reads from a file and produces Tokens.
--
-- This assumes that the files are UTF8.
tokenizeFileStream :: FilePath -> E.Enumerator Token IO b
tokenizeFileStream inputFile =
    EB.enumFile inputFile E.$=
    ET.decode ET.utf8 E.$=
    tokenizeStream inputFile 0

-- | This is an Enumeratee that takes a FilePath and returns a Enumerator of
-- Tokens.
tokenizeE :: E.Enumeratee FilePath Token IO b
tokenizeE cont@(E.Continue _) = do
    maybeFP <- EL.head
    case maybeFP of
        Just filePath -> tokenizeFileStream filePath cont E.>>== tokenizeE
        Nothing -> return cont
tokenizeE step = return step

-- | This is an Enumeratee that takes a stream of Char and transforms it into a
-- stream of Token.
tokenizeStream :: Monad m =>
                  FilePath -> Integer -> E.Enumeratee T.Text Token m b
tokenizeStream source offset cont@(E.Continue k) = do
    maybeC <- ET.head
    case maybeC of
        Just c  -> do
            token <- tokenize' source offset c
            next  <- lift $ E.runIteratee $ k $ E.Chunks [token]
            tokenizeStream source (offset + fromIntegral (tokenLength token)) next
        Nothing -> return cont
tokenizeStream _ _ step = return step

-- | This takes a character and dispatches the handle the tokenizing the rest
-- of the token from it.
tokenize' :: Monad m => FilePath -> Integer -> Char -> E.Iteratee T.Text m Token
tokenize' source offset c
    | C.isAlpha c       = tokenFromTaken source offset AlphaToken c C.isAlpha
    | C.isNumber c      = tokenFromTaken source offset NumberToken c C.isNumber
    | isSeparator c     = tokenFromTaken source offset SeparatorToken c isSeparator
    | C.isPunctuation c = return . makeToken source offset PunctuationToken $ T.singleton c
    | C.isSymbol c      = return . makeToken source offset SymbolToken $ T.singleton c
    | C.isMark c        = return . makeToken source offset MarkToken $ T.singleton c
    | otherwise         = return . makeToken source offset UnknownToken $ T.singleton c

-- | This is an augmented separator predicate that also tests for spaces.
isSeparator :: Char -> Bool
isSeparator c = C.isSpace c || C.isSeparator c

-- | This runs takeWhile with the predicate, conses the initial element to the
-- front, and creates a Token of the given type.
tokenFromTaken :: Monad m   
               => FilePath                      -- ^ tokenSource
               -> Integer                       -- ^ tokenOffset
               -> TokenType                     -- ^ tokenType
               -> Char                          -- ^ Initial character.
               -> (Char -> Bool)                -- ^ Predicate for taking the
                                                -- rest of the token.
               -> E.Iteratee T.Text m Token
tokenFromTaken source offset tType initial predicate =
    liftM (makeToken source offset tType . LT.toStrict . LT.cons initial)
          (ET.takeWhile predicate)

-- | In the context of an Enumerator, this takes a [Char] list and returns a
-- Token.
makeToken :: FilePath -> Integer -> TokenType -> T.Text -> Token
makeToken source offset tType raw =
    Token normalized raw rawLength tType source offset
    where
        normalized = normalizeToken raw
        rawLength  = T.length raw

-- | This takes a raw token Text and returns a normalized version. Currently,
-- this just lower-cases everything.
normalizeToken :: T.Text -> T.Text
normalizeToken = T.map C.toLower

