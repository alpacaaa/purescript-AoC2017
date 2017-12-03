module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Day3 as Day3


main = do
  solution <- Day3.solution
  log $ show solution
