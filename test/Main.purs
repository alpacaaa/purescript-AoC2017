module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)


import Day9 as Day9

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  Day9.test
