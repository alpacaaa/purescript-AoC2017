module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)


import Day5 as Day5

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  Day5.test
