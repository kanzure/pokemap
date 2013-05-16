-- Copyright © 2013 Julian Blake Kongslie <jblake@omgwallhack.org>
-- Licensed under the MIT license.

{-# LANGUAGE PackageImports #-}

module Maze
where

import Control.Applicative
import Control.DeepSeq
import Control.Monad
import Control.Monad.Logic
import Control.Monad.Random
import Control.Monad.State.Strict
import Data.Array
import Data.Maybe
import System.Random
import System.Random.Shuffle

import Block

instance (MonadRandom m) => MonadRandom (LogicT m) where
  getRandom = lift getRandom
  getRandoms = lift getRandoms
  getRandomR = lift . getRandomR
  getRandomRs = lift . getRandomRs

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
buildMaze edge (xmax, ymax) bs = fmap fromJust <$> execStateT (observeT buildCell) initMap
  where

    initMap = listArray ((1, 1), (xmax, ymax)) $ repeat Nothing

    getColor s p = do
      a <- lift get
      if inRange (bounds a) p
        then case a ! p of
          Nothing -> return $ const True
          Just b -> return $ \b' -> sideColors b ! s == sideColors b' ! (opposite s)
        else return $ \b' -> edge == sideColors b' ! (opposite s)

    buildCell = do

      m <- lift $ get

      case filter (\p -> isNothing $ m ! p) $ indices m of
        [] -> return ()
        (p:_) -> do

          case p of
            (x,1) -> liftIO $ putStrLn $ "Approximately " ++ show (round $ (100::Float) * fromIntegral (x-1) / fromIntegral xmax) ++ "% complete."
            _ -> return ()

          eastColor  <- getColor West  $ east  p
          northColor <- getColor South $ north p

          bs' <- shuffleM $ filter eastColor $ filter northColor bs 

          bs' `deepseq` case bs' of

            [] -> do
              liftIO $ putStrLn $ "Backtracking at " ++ show p
              mzero

            _ -> do
              b <- msum $ map return bs'
              m' <- lift $ force <$> gets (// [(p, Just b)])
              lift $ put m'

          buildCell
