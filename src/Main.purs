module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Day7 as Day7


main = do
  solution <- Day7.solution unit
  log $ show solution
