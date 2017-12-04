module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Day4 as Day4


main = do
  solution <- Day4.solution unit
  log $ show solution
