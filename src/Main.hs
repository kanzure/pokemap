-- Copyright © 2013 Julian Blake Kongslie <jblake@omgwallhack.org>
-- Licensed under the MIT license.

module Main
where

import Control.Monad.Random
import Data.Array
import System.Environment

import Block
import Maze

main :: IO ()
main = do

  args <- getArgs

  (xmax, ymax, blockFile) <- case args of
    [xstr, ystr, blockFile] -> return (read xstr, read ystr, blockFile)
    [xstr, ystr]            -> return (read xstr, read ystr, "data/testBlocks")
    []                      -> return (50,        20,        "data/testBlocks")
    _                       -> fail "Must provide either zero, two, or three arguments!"

  bs <- readBlockFile blockFile

  putStrLn $ show (length bs) ++ " blocks loaded."

  let

    initMap = listArray ((1,1),(xmax,ymax)) $ repeat Nothing

    colorM (x, _) West | x == xmax + 1 = Just "rock"
    colorM (0, _) East                 = Just "water"
    colorM _      _                    = Nothing

  g <- newStdGen

  m <- evalRandT (buildMaze initMap colorM bs) g

  mapM_ putStrLn $ ppMapMovement m
  putStrLn ""
  mapM_ putStrLn $ ppMapTiles m
