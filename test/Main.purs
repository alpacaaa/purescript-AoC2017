module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)


import Day1 as Day1

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  Day1.test
