module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Day6 as Day6


main = do
  solution <- Day6.solution unit
  log $ show solution
