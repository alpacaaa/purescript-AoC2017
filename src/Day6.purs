module Day6 where

import Prelude

import Control.Monad.Eff.Console (log)
import Data.Maybe
import Data.Array as Array
import Data.String as String
import Data.Int as Int
import Data.Set as Set



import Data.Traversable (traverse)

import Node.FS.Sync as FS
import Node.Encoding as Encoding



newtype Index =
    Index Int


solution _ = do
    pure $ solve [4, 1, 15, 12, 0, 9, 9, 5, 5, 8, 7, 3, 14, 5, 12, 3]

print input expected = do
    log $ "Solving " <> (show input) <> ": " <> (show $ solve input) <> " expecting: " <> (show expected)



loop :: Array Int -> Array Int
loop input =
    let
        folder n max =
            if n > max then n else max

        max =
            Array.foldl folder 0 input

        index =
            Array.findIndex (_ == max) input
            # maybe 0 id

    in
    Array.updateAt index 0 input
    # maybe [] id
    # solve' (next (Index index) input) max


solve :: Array Int -> Int
solve input =
    let
        findIteration list result =
            let
                current = loop list
            in
            if Set.member current result then
                (Set.size result) + 1
            else
                findIteration current (Set.insert current result)
    in
    findIteration input Set.empty


solve' :: Index -> Int -> Array Int -> Array Int
solve' (Index index) 0         list =
    list

solve' theIndex@(Index index) remaining list =
    Array.modifyAt index (_ + 1) list
    # maybe [] id
    # solve' (next theIndex list) (remaining - 1)



next :: Index -> Array Int -> Index
next (Index index) list =
    Index $
        if Array.length list == (index + 1)
            then 0
            else index + 1


test = do
    print [0, 2, 7, 0] 5
    pure unit

