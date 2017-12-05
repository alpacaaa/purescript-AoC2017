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

newtype Step =
    Step Int

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
    # solve' (Step 0) 0


solve' :: Step -> Int -> Array Instruction ->  Int
solve' (Step step) index stack =
    case Array.index stack index of
        Just (Instruction instruction) ->
            let
                updatedStack =
                    stack
                    # Array.updateAt index (Instruction $ instruction + 1)
                    # maybe [] id
            in
            solve' (Step $ step + 1) (index + instruction) updatedStack

        Nothing ->
            step


test = do
    print ["0", "3", "0", "1", "-3"] 5
    pure unit