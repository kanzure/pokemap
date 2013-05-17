-- Copyright © 2013 Julian Blake Kongslie <jblake@omgwallhack.org>
-- Licensed under the MIT license.

{-# OPTIONS_GHC -Wall -Werror -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies #-}

module Maze
where

import Control.Applicative
import Control.DeepSeq
import Control.Monad
import Control.Monad.Random
import Control.Monad.State.Strict
import Data.Array
import Data.List
import Data.Maybe
import System.Random.Shuffle

import Block

instance (MonadRandom m) => MonadRandom (StateT s m) where
  getRandom = lift getRandom
  getRandoms = lift getRandoms
  getRandomR = lift . getRandomR
  getRandomRs = lift . getRandomRs

east :: (Int, Int) -> (Int, Int)
east  (x,y) = (x+1,y)

north :: (Int, Int) -> (Int, Int)
north (x,y) = (x,y-1)

south :: (Int, Int) -> (Int, Int)
south (x,y) = (x,y+1)

west :: (Int, Int) -> (Int, Int)
west  (x,y) = (x-1,y)

buildMaze :: (Eq c, NFData c, Ord c) => Array (Int, Int) (Maybe (Block c)) -> ((Int, Int) -> Side -> [c]) -> [Block c] -> RandT StdGen IO (Map c)
buildMaze initMap colorM bs = fmap (fromMaybe $ head bs) <$> execStateT (buildCell 0 $ indices initMap) initMap
  where

    getColor s p = case colorM p s of
      [] -> do
        a <- get
        if inRange (bounds a) p
          then case a ! p of
            Just b  -> return $ \b' -> sideColors b ! s == [] || sideColors b' ! (opposite s) == [] || or [ c == c' | c <- sideColors b ! s, c' <-  sideColors b' ! (opposite s) ]
            Nothing -> return $ const True
          else return $ const True
      cs -> return $ \b' -> sideColors b' ! (opposite s) == [] || or [ c == c' | c <- cs, c' <- sideColors b' ! (opposite s) ]

    killCell p x = do
      m <- get
      if inRange (bounds m) p
        then case initMap ! p of
          Just _  -> x
          Nothing -> case m ! p of
            Just _  -> put $ force $ m // [(p, Nothing)]
            Nothing -> x
        else x

    buildCell _ [] = return ()
    buildCell n (p:q) = do

      done <- gets $ \a -> not (inRange (bounds a) p) || isJust (a ! p)

      if done
        then buildCell n q
        else do

          let q' = if n == 0 then force $ nub q else q

          when (n == (0 :: Int)) $ do
            m <- get
            liftIO $ mapM_ putStrLn $ ppMapMovement $ fmap (fromMaybe $ head bs) m
            liftIO $ putStrLn ""

          eastColor  <- getColor West  $ east  p
          northColor <- getColor South $ north p
          southColor <- getColor North $ south p
          westColor  <- getColor East  $ west  p

          bs' <- shuffleM $ filter eastColor $ filter northColor $ filter southColor $ filter westColor bs
          if length bs' == length bs
            then buildCell n $ q' ++ [p]
            else case force bs' of

              [] -> do
                modify $ \m -> force $ m // [(p, Nothing)]
                kills <- shuffleM [east, north, south, west]
                foldr (\d x -> killCell (d p) x) (return ()) kills
                buildCell ((n+1) `mod` 100) $ p : east p : north p : south p : west p : q'

              (b:_) -> do
                modify $ \m -> force $ m // [(p, Just b)]
                buildCell ((n+1) `mod` 100) $ q' ++ [east p, north p, south p, west p]
