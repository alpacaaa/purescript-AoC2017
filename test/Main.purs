module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)


import Day3 as Day3

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  Day3.test
