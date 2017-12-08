module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)


import Day8 as Day8

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  Day8.test
