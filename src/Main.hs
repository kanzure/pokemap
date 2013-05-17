-- Copyright © 2013 Julian Blake Kongslie <jblake@omgwallhack.org>
-- Licensed under the MIT license.

{-# OPTIONS_GHC -Wall -Werror #-}

module Main
where

import Control.Applicative
import Control.DeepSeq
import Control.Monad.Random
import Data.Array
import Data.List
import System.Environment

import Block
import Maze

import Paths_pokemap

filePath :: String -> IO String
filePath fp | '/' `elem` fp = return fp
            | otherwise     = getDataFileName fp

main :: IO ()
main = do

  args <- getArgs

  (xmax, ymax, blockFile) <- case args of
    [xstr, ystr, blockFile] -> filePath blockFile     >>= \f -> return (read xstr, read ystr, f)
    [xstr, ystr]            -> filePath "test.blocks" >>= \f -> return (read xstr, read ystr, f)
    []                      -> filePath "test.blocks" >>= \f -> return (50,        20,        f)
    _                       -> fail "Must provide either zero, two, or three arguments!"

  bs <- readBlockFile blockFile

  putStrLn $ show (length bs) ++ " blocks loaded."

  let colors = nub $ concatMap (concat . elems . sideColors) bs

  putStrLn $ show (length colors) ++ " colors defined: " ++ show colors

  let

    initMap = listArray ((1,1),(xmax,ymax)) $ repeat Nothing

    colorM (x, _) West  | x == xmax + 1 = ["rock"]
    colorM (0, _) East                  = ["water"]
    colorM (_, y) North | y == ymax + 1 = ["water", "watersand", "sandwater", "sand", "sanddirt", "dirtsand", "dirt", "dirtgrass", "grassdirt", "grass", "rock", "rockgrass", "grassrock", "rockdirt", "dirtrock", "rocksand", "sandrock"]
    colorM (_, 0) South                 = ["water", "watersand", "sandwater", "sand", "sanddirt", "dirtsand", "dirt", "dirtgrass", "grassdirt", "grass", "rock", "rockgrass", "grassrock", "rockdirt", "dirtrock", "rocksand", "sandrock"]
    colorM _      _                     = []

  g <- newStdGen

  m <- force <$> evalRandT (buildMaze initMap colorM bs) g

  mapM_ putStrLn $ ppMapMovement m
  putStrLn ""
  mapM_ putStrLn $ ppMapTiles m
