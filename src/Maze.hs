-- Copyright © 2013 Julian Blake Kongslie <jblake@omgwallhack.org>
-- Licensed under the MIT license.

{-# LANGUAGE PackageImports #-}

module Maze
where

import Control.Applicative
import Control.DeepSeq
import Control.Monad
import Control.Monad.Random
import "monads-tf" Control.Monad.State.Strict
import Data.Array
import Data.Maybe
import System.Random
import System.Random.Shuffle

import Block

instance (MonadRandom m) => MonadRandom (StateT s m) where
  getRandom = lift getRandom
  getRandoms = lift getRandoms
  getRandomR = lift . getRandomR
  getRandomRs = lift . getRandomRs

east  (x,y) = (x+1,y)
north (x,y) = (x,y-1)
south (x,y) = (x,y+1)
west  (x,y) = (x-1,y)

buildMaze :: (Eq c, NFData c) => c -> (Int, Int) -> [Block c] -> RandT StdGen IO (Map c)
buildMaze edge (xmax, ymax) bs = fmap fromJust <$> execStateT buildCell initMap
  where

    initMap = listArray ((1, 1), (xmax, ymax)) $ repeat Nothing

    getColor s p = do
      a <- get
      if inRange (bounds a) p
        then case a ! p of
          Nothing -> return $ const True
          Just b -> return $ \b' -> sideColors b ! s == sideColors b' ! (opposite s)
        else return $ \b' -> edge == sideColors b' ! (opposite s)

    killCell p = do
      a <- get
      if inRange (bounds a) p
        then put $ a // [(p, Nothing)]
        else return ()

    buildCell = do

      m <- get

      case filter (\p -> isNothing $ m ! p) $ indices m of
        [] -> return ()
        (p:_) -> do

          eastColor  <- getColor West  $ east  p
          northColor <- getColor South $ north p
          southColor <- getColor North $ south p
          westColor  <- getColor East  $ west  p

          bs' <- shuffleM $ filter eastColor $ filter northColor $ filter southColor $ filter westColor bs

          case bs' of

            [] -> do
              killCell $ east  p
              killCell $ north p
              killCell $ south p
              killCell $ west  p
              m' <- get
              m `deepseq` return ()

            (b:_) -> do
              m' <- gets (// [(p, Just b)])
              m' `deepseq` put m'

          buildCell
