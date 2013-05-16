-- Copyright © 2013 Julian Blake Kongslie <jblake@omgwallhack.org>
-- Licensed under the MIT license.

module Main
where

import Control.Monad.Random

import Block
import Maze

main :: IO ()
main = do

  bs <- readBlockFile "data/testBlocks"

  g <- newStdGen

  m <- evalRandT (buildMaze "wall" (360,400) bs) g

  mapM_ putStrLn $ ppMapMovement m
  mapM_ putStrLn $ ppMapTiles m
