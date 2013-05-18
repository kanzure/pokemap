-- Copyright © 2013 Julian Blake Kongslie <jblake@omgwallhack.org>
-- Licensed under the MIT license.

import Distribution.Simple
import System.Exit
import System.Process

main :: IO ()
main = defaultMainWithHooks $ simpleUserHooks
  { postBuild = \_ _ _ _ -> makeDataFiles
  , postClean = \_ _ _ _ -> cleanDataFiles
  }

makeDataFiles :: IO ()
makeDataFiles = rawSystem "make" ["-C", "data"] >>= exitWith

cleanDataFiles :: IO ()
cleanDataFiles = rawSystem "make" ["-C", "data", "clean"] >>= exitWith
