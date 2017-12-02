module Day2 where

import Data.Maybe
import Prelude

import Control.Monad.Eff.Console (log)
import Data.Array as Array
import Data.Foldable (sum)
import Data.Int as Int
import Data.String as String
import Data.Traversable (traverse)

import Node.FS.Sync as FS
import Node.Encoding as Encoding




solution = do
    input <- FS.readTextFile Encoding.UTF8 "input/day2.txt"

    pure $
        String.split (String.Pattern "\n") input
            # map (String.split $ String.Pattern "\t")
            # solve

print input expected = do
    log $ "Solving " <> (show input) <> ": " <> (show $ solve input) <> " expecting: " <> (show expected)


type Row = { min :: Int, max :: Int }


solve :: Array (Array String) -> Int
solve input =
    let
        mapper numbers =
            traverse Int.fromString numbers
            # maybe 0 solve'
    in
    map mapper input
    # sum


solve' :: Array Int -> Int
solve' input =
    let
        fold initial =
            Array.foldl solveRow { min: initial, max: initial } input
    in
    Array.head input
    # map fold
    # maybe 0 (\{ min, max} -> max - min)


solveRow :: Row -> Int -> Row
solveRow { min, max } n
    | n > max   = { min,    max: n }
    | n < min   = { min: n, max    }
    | otherwise = { min,    max    }


test = do
    print [["5", "1", "9", "5"]] 8
    print [["7", "5", "3"]]      4
    print [["2", "4", "6", "8"]] 6
    pure unit