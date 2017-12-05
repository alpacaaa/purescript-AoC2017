module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Day5 as Day5


main = do
  solution <- Day5.solution unit
  log $ show solution
