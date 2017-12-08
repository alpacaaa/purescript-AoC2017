module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Day8 as Day8


main = do
  solution <- Day8.solution unit
  log $ show solution
