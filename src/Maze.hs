-- Copyright © 2013 Julian Blake Kongslie <jblake@omgwallhack.org>
-- Licensed under the MIT license.

{-# LANGUAGE PackageImports #-}

module Maze
where

import Control.Applicative
import Control.Monad
import Control.Monad.Random
import "monads-tf" Control.Monad.State
import Data.Array
import Data.Maybe
import System.Random
import System.Random.Shuffle

import Block

east  (x,y) = (x+1,y)
north (x,y) = (x,y-1)
south (x,y) = (x,y+1)
west  (x,y) = (x-1,y)

buildMaze :: (Eq c) => c -> (Int, Int) -> [Block c] -> RandT StdGen IO (Map c)
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
        then return [put $ a // [(p, Nothing)]]
        else return []

    buildCell = do

      as <- gets assocs

      case filter (\(p,b) -> isNothing b) as of
        [] -> return ()
        ((p,_):_) -> do

          eastColor  <- getColor West  $ east  p
          northColor <- getColor South $ north p
          southColor <- getColor North $ south p
          westColor  <- getColor East  $ west  p

          bs' <- shuffleM $ filter eastColor $ filter northColor $ filter southColor $ filter westColor bs

          case bs' of

            [] -> do
              killEast  <- killCell $ east  p
              killNorth <- killCell $ north p
              killSouth <- killCell $ south p
              killWest  <- killCell $ west  p
              kills <- shuffleM $ killEast ++ killNorth ++ killSouth ++ killWest
              head kills
              buildCell

            (b:_) -> do
              modify $ \m -> m // [(p,Just b)]
              buildCell
