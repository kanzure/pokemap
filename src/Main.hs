-- Copyright © 2013 Julian Blake Kongslie <jblake@omgwallhack.org>
-- Licensed under the MIT license.

module Main
where

import Data.Functor.Identity
import Control.Monad.Random
import System.Random

import Block
import Maze

main :: IO ()
main = do

  bs <- readBlockFile "data/testBlocks"

  g <- newStdGen

  m <- evalRandT (buildMaze "wall" (400,360) bs) g

  mapM_ putStrLn $ ppMapMovement m
  mapM_ putStrLn $ ppMapTiles m
