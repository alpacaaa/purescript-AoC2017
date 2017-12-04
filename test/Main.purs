module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)


import Day4 as Day4

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  Day4.test
