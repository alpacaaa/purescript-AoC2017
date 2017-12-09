module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Day9 as Day9


main = do
  solution <- Day9.solution unit
  log $ show solution
