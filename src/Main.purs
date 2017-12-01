module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Day1 as Day1


main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log $ show Day1.solution
