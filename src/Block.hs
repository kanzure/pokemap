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
import qualified Text.Parsec as P
import qualified Text.Parsec.Perm as P

type MovementClass = Char

parseMovementClass :: (P.Stream s m Char) => P.ParsecT s u m MovementClass
parseMovementClass = P.anyChar

ppMovementClass :: MovementClass -> [String]
ppMovementClass t = [[t]]

type Tile = Char

parseTile :: (P.Stream s m Char) => P.ParsecT s u m Tile
parseTile = P.anyChar

ppTile :: Tile -> [String]
ppTile t = [[t]]

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
  { movementArray :: !(Array (Int, Int) MovementClass)
  , tileArray :: !(Array (Int, Int) Tile)
  , sideColors :: !(Array Side [c])
  }
  deriving (Eq, Ord, Read, Show)

instance (NFData c) => NFData (Block c) where
  rnf b = movementArray b `deepseq` tileArray b `deepseq` sideColors b `deepseq` ()

blockMovementSize :: (Int, Int)
blockMovementSize = (2, 2)

blockTileSize :: (Int, Int)
blockTileSize = (4, 4)

parseArray :: (P.Stream s m Char) => P.ParsecT s u m x -> (Int, Int) -> P.ParsecT s u m (Array (Int, Int) x)
parseArray parseElement (xmax, ymax) = do

  ess <- forM [1..ymax] $ \y -> do
    es <- forM [1..xmax] $ \x -> do
      e <- parseElement
      return ((x, y), e)
    _ <- P.newline
    return es

  return $ array ((1, 1), (xmax, ymax)) $ concat ess

ppNested :: [[String]] -> [String]
ppNested vss = do
  i <- [0 .. length (vss !! 0) - 1]
  return $ concat $ concat [ take 1 $ drop i vs | vs <- vss ]

ppArray :: Array (Int, Int) [String] -> [String]
ppArray a = concat [ ppNested [ a ! (x,y) | x <- [xmin..xmax] ] | y <- [ymin..ymax] ]
  where
    ((xmin,ymin),(xmax,ymax)) = bounds a

ppBlockMovement :: Block c -> [String]
ppBlockMovement b = ppArray $ fmap ppMovementClass $ movementArray b

ppBlockTiles :: Block c -> [String]
ppBlockTiles b = ppArray $ fmap ppTile $ tileArray b

mkColorFunc :: c -> c -> c -> c -> Array Side c
mkColorFunc e n s w = listArray (minBound, maxBound) [e,n,s,w]

parseBlock :: (P.Stream s Identity Char) => P.ParsecT s u Identity (Block String)
parseBlock = do

  let
    parseColor = P.many1 $ P.satisfy $ \c -> not $ isPunctuation c || isSpace c || isSymbol c
    parseSide s = do
      _ <- P.string s
      xs <- (P.sepBy1 parseColor (P.char '|')) P.<|> (P.char '*' >> return [])
      _ <- P.many $ P.satisfy $ \c -> c /= '\n' && isSpace c
      return xs

  colorFunc <- P.permute $ mkColorFunc
    P.<$$> parseSide "e:"
    P.<||> parseSide "n:"
    P.<||> parseSide "s:"
    P.<||> parseSide "w:"

  _ <- P.newline

  movement <- parseArray parseMovementClass blockMovementSize
  tiles <- parseArray parseTile blockTileSize

  return $ Block
    { movementArray = movement
    , tileArray = tiles
    , sideColors = colorFunc
    }

readBlockFile :: String -> IO [Block String]
readBlockFile file = do

  d <- readFile file

  case P.parse (P.many parseBlock <* P.eof) file d of
    Left e -> fail $ show e
    Right bs -> return bs

type Map c = Array (Int, Int) (Block c)

ppMapMovement :: Map c -> [String]
ppMapMovement m = ppArray $ fmap ppBlockMovement m

ppMapTiles :: Map c -> [String]
ppMapTiles m = ppArray $ fmap ppBlockTiles m
