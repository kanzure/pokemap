-- Copyright © 2013 Julian Blake Kongslie <jblake@omgwallhack.org>
-- Licensed under the MIT license.

{-# LANGUAGE FlexibleContexts #-}

module Block
where

import Control.Applicative
import Control.Monad
import Data.Array
import Data.Char
import Data.Functor.Identity
import qualified Text.Parsec as P
import qualified Text.Parsec.Perm as P

data MovementClass
  = Passable
  | Impassable
  | BlocksEast
  | BlocksNorthEast
  | BlocksNorth
  | BlocksNorthWest
  | BlocksWest
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

parseMovementClass :: (P.Stream s m Char) => P.ParsecT s u m MovementClass
parseMovementClass = P.choice
  [ P.char '.'  *> pure Passable
  , P.char '#'  *> pure Impassable
  , P.char '>'  *> pure BlocksEast
  , P.char '/'  *> pure BlocksNorthEast
  , P.char 'v'  *> pure BlocksNorth
  , P.char '\\' *> pure BlocksNorthWest
  , P.char '<'  *> pure BlocksWest
  ]

ppMovementClass :: MovementClass -> [String]
ppMovementClass Passable        = ["."]
ppMovementClass Impassable      = ["#"]
ppMovementClass BlocksEast      = [">"]
ppMovementClass BlocksNorthEast = ["/"]
ppMovementClass BlocksNorth     = ["v"]
ppMovementClass BlocksNorthWest = ["\\"]
ppMovementClass BlocksWest      = ["<"]

type Tile = Char

parseTile :: (P.Stream s m Char) => P.ParsecT s u m Tile
parseTile = P.anyChar

ppTile :: Tile -> [String]
ppTile t = [[t]]

data Side
  = East | North | South | West
  deriving (Bounded, Enum, Eq, Ix, Ord, Read, Show)

opposite :: Side -> Side
opposite East  = West
opposite North = South
opposite South = North
opposite West  = East

data Block c = Block
  { movementArray :: !(Array (Int, Int) MovementClass)
  , tileArray :: !(Array (Int, Int) Tile)
  , sideColors :: !(Array Side c)
  }
  deriving (Eq, Ord, Read, Show)

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
    P.newline
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
    parseSide s = do
      P.string s
      x <- P.many $ P.satisfy $ not . isSpace
      P.many $ P.satisfy $ \c -> c /= '\n' && isSpace c
      return x

  colorFunc <- P.permute $ mkColorFunc
    P.<$$> parseSide "e:"
    P.<||> parseSide "n:"
    P.<||> parseSide "s:"
    P.<||> parseSide "w:"

  P.newline

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
