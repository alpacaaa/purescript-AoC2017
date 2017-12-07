module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)


import Day7 as Day7

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  Day7.test
