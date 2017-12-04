module Day4 where

import Prelude

import Control.Monad.Eff.Console (log)
import Data.Array as Array
import Data.String as String
import Data.Set as Set

import Node.FS.Sync as FS
import Node.Encoding as Encoding


solution _ = do
    input <- FS.readTextFile Encoding.UTF8 "input/day4.txt"

    pure $
        String.split (String.Pattern "\n") input
            # solve

print input expected = do
    log $ "Solving " <> (show input) <> ": " <> (show $ solveRow input) <> " expecting: " <> (show expected)


solve :: Array String -> Int
solve input =
    Array.filter solveRow input
    # Array.length


solveRow :: String -> Boolean
solveRow phrase =
    let
        split =
            String.split (String.Pattern " ") phrase

        unique =
            Set.fromFoldable split
    in
    (Array.length split) == (Set.size unique)




test = do
    print "aa bb cc dd ee"  true
    print "aa bb cc dd aa"  false
    print "aa bb cc dd aaa" true
    pure unit