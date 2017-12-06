module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)


import Day6 as Day6

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  Day6.test
