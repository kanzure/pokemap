-- Copyright © 2013 Julian Blake Kongslie <jblake@omgwallhack.org>
-- Licensed under the MIT license.

{-# OPTIONS_GHC -Wall -Werror #-}

module Main
where

import Control.Applicative
import Control.DeepSeq
import Control.Monad
import Control.Monad.Random
import Data.Array
import Data.Bits
import qualified Data.ByteString as BS
import Data.Function
import Data.List
import System.Environment

import Block
import Maze

import Paths_pokemap

filePath :: String -> IO String
filePath fp | '/' `elem` fp = return fp
            | otherwise     = getDataFileName fp

duplicates :: (Eq a) => [a] -> [a]
duplicates []                       =     []
duplicates [_]                      =     []
duplicates (x:xs@(y:_)) | x == y    = x : duplicates xs
                        | otherwise =     duplicates xs

main :: IO ()
main = do

  args <- getArgs

  (xmax, ymax, blockFile, outFile) <- case args of
    [xstr, ystr, blockFile, outFile] -> filePath blockFile     >>= \f -> return (read xstr, read ystr, f, outFile)
    [xstr, ystr, blockFile]          -> filePath blockFile     >>= \f -> return (read xstr, read ystr, f, "out.map")
    [xstr, ystr]                     -> filePath "test.blocks" >>= \f -> return (read xstr, read ystr, f, "out.map")
    []                               -> filePath "test.blocks" >>= \f -> return (20,        20,        f, "out.map")
    _                                -> fail "Usage: pokemap [x y [blockfile [outputfile]]]"

  putStrLn $ "Loading blocks from " ++ show blockFile ++ "."

  bs <- sortBy (compare `on` blockIndex) <$> readBlockFile blockFile

  putStrLn $ show (length bs) ++ " blocks loaded."

  let dups = nub $ duplicates $ map blockIndex bs

  when (not $ null dups) $ putStrLn $ "Warning: Duplicate block indexes: " ++ intercalate ", " (map show dups)

  let colors = nub $ sort $ concatMap (concat . elems . sideColors) bs

  putStrLn $ show (length colors) ++ " colors defined: " ++ intercalate ", " colors

  let

    initMap = listArray ((1,1),(xmax,ymax)) $ repeat Nothing

--    colorM (x, _) West  | x == xmax + 1 = ["rock"]
--    colorM (0, _) East                  = ["water"]
--    colorM (_, y) North | y == ymax + 1 = ["water", "watersand", "sandwater", "sand", "sanddirt", "dirtsand", "dirt", "dirtgrass", "grassdirt", "grass", "rock", "rockgrass", "grassrock", "rockdirt", "dirtrock", "rocksand", "sandrock"]
--    colorM (_, 0) South                 = ["water", "watersand", "sandwater", "sand", "sanddirt", "dirtsand", "dirt", "dirtgrass", "grassdirt", "grass", "rock", "rockgrass", "grassrock", "rockdirt", "dirtrock", "rocksand", "sandrock"]
    colorM _      _                     = []

  g <- newStdGen

  putStrLn $ "Okay, generating a " ++ show xmax ++ "x" ++ show ymax ++ " block map..."

  mm <- force <$> evalRandT (buildMaze initMap colorM bs) g

  case mm of
    Nothing -> putStrLn "Failed to find a solution. Not sure how that happened, I should've just run forever trying. Probably a bug."
    Just m -> do

      putStrLn $ "Saving result to " ++ show outFile ++ "."

      let bstr = BS.pack $ [ fromIntegral $ xmax `shiftR` 8, fromIntegral xmax, fromIntegral $ ymax `shiftR` 8, fromIntegral ymax ] ++ [ blockIndex $ m ! (x,y) | y <- [1..ymax], x <- [1..xmax] ]

      BS.writeFile outFile bstr
