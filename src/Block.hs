-- Copyright © 2013 Julian Blake Kongslie <jblake@omgwallhack.org>
-- Licensed under the MIT license.

{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE FlexibleContexts #-}

module Block
where

import Control.Applicative
import Control.DeepSeq
import Control.Monad
import Data.Array
import Data.Char
import Data.Functor.Identity
import qualified Data.Traversable as T
import Data.Word
import qualified Text.Parsec as P
import qualified Text.Parsec.Perm as P

data Side
  = East | North | South | West
  deriving (Bounded, Enum, Eq, Ix, Ord, Read, Show)

instance NFData Side where
  rnf s = s `seq` ()

opposite :: Side -> Side
opposite East  = West
opposite North = South
opposite South = North
opposite West  = East

data Block c = Block
  { blockIndex :: !Word8
  , sideColors :: !(Array Side [c])
  }
  deriving (Eq, Ord, Read, Show)

instance (NFData c) => NFData (Block c) where
  rnf b = rnf $ blockIndex b `deepseq` sideColors b

mkColorFunc :: c -> c -> c -> c -> Array Side c
mkColorFunc e n s w = listArray (minBound, maxBound) [e,n,s,w]

spaces :: (P.Stream s m Char) => P.ParsecT s u m ()
spaces = do
  _ <- P.many $ P.satisfy $ \c -> c /= '\n' && isSpace c
  return ()

parseBlock :: (P.Stream s Identity Char) => P.ParsecT s u Identity (Block String)
parseBlock = do

  let

    parseColor = P.many1 $ P.satisfy $ \c -> not $ isPunctuation c || isSpace c || isSymbol c
    parseColors = (P.sepBy1 parseColor (P.char '|')) P.<|> (P.char '*' >> return [])
    parseIndex = do
      i <- read <$> P.many1 P.digit
      guard $ i < (256 :: Int)
      return $ fromIntegral i

    parseOption s p = do
      _ <- P.string s
      _ <- P.char ':'
      x <- p
      spaces
      return x

    mkBlock i e n s w = Block
      { blockIndex = i
      , sideColors = listArray (minBound, maxBound) [e,n,s,w]
      }

  P.spaces
  _ <- P.string "block"
  spaces

  block <- P.permute $ mkBlock
    P.<$$> parseOption "index" parseIndex
    P.<||> parseOption "east"  parseColors
    P.<||> parseOption "north" parseColors
    P.<||> parseOption "south" parseColors
    P.<||> parseOption "west"  parseColors

  _ <- P.newline

  return block

readBlockFile :: String -> IO [Block String]
readBlockFile file = do

  d <- readFile file

  case P.parse (P.many parseBlock <* P.eof) file d of
    Left e -> fail $ show e
    Right bs -> return bs

type PartialMap c = Array (Int, Int) (Maybe (Block c))
type Map c = Array (Int, Int) (Block c)

unpartial :: PartialMap c -> Maybe (Map c)
unpartial = T.sequenceA
