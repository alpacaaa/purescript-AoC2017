module Day5 where

import Prelude

import Control.Monad.Eff.Console (log)
import Data.Maybe
import Data.Array as Array
import Data.String as String
import Data.Int as Int
import Data.Traversable (traverse)

import Node.FS.Sync as FS
import Node.Encoding as Encoding


newtype Instruction =
    Instruction Int


solution _ = do
    input <- FS.readTextFile Encoding.UTF8 "input/day5.txt"

    pure $
        String.split (String.Pattern "\n") input
            # solve

print input expected = do
    log $ "Solving " <> (show input) <> ": " <> (show $ solve input) <> " expecting: " <> (show expected)


solve :: Array String -> Int
solve input =
    traverse Int.fromString input
    # maybe [] (map Instruction)
    # solve' 0


solve' :: Int -> Array Instruction ->  Int
solve' index stack =
    case Array.index stack index of
        Just (Instruction instruction) ->
            stack
            # Array.updateAt index (Instruction $ instruction + 1)
            # maybe 0 (solve' $ index + instruction)

        Nothing ->
            index


test = do
    print ["0", "3", "0", "1", "-3"] 5
    pure unit