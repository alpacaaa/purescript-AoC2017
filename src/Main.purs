module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Day2 as Day2


main = do
  solution <- Day2.solution
  log $ show solution
