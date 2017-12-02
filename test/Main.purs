module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)


import Day2 as Day2

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  Day2.test
