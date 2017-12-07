module Day7 where

import Prelude

import Control.Monad.Eff.Console (log)
import Data.Maybe
import Data.Array as Array
import Data.String as String
import Data.Int as Int
import Data.Map (Map(..))
import Data.Map as Map
import Data.List as List


import Debug.Trace as Debug
import Data.Traversable (traverse)

import Node.FS.Sync as FS
import Node.Encoding as Encoding


solution _ = do
    input <- FS.readTextFile Encoding.UTF8 "input/day7.txt"
    pure $ maybe "it's broken" id (solve input)


print input expected = do
    log $ (show $ solve input) <> " expecting: " <> expected


type Row =
    { program :: String, weight :: String, children :: Array String }


type Result =
    Map String Int


solve :: String -> Maybe String
solve input =
    input
    #   String.split (String.Pattern "\n")
    #   traverse createRow
    #   map (Array.foldl solve' Map.empty)
    >>= findResult


solve' :: Result -> Row -> Result
solve' result row =
    let
        alter current =
            case current of
                Just n  -> Just (n + 1)
                Nothing -> Just 1

        update acc child =
            Map.alter alter child acc

        newResult =
            if Array.null row.children then
                result
            else
                Map.alter (maybe (Just 0) Just) row.program result
    in
    Array.foldl update newResult row.children


findResult :: Result -> Maybe String
findResult result =
    Map.filter (_ == 0) result
    # Map.keys
    # List.head

createRow :: String -> Maybe Row
createRow input = do
    let
        parts =
            String.split (String.Pattern " -> ") input

    program <- (Array.index parts 0) >>= parseProgram
    weight  <- (Array.index parts 0) >>= parseWeight

    let
        children =
            Array.index parts 1
            # maybe [] parseChildren

    pure { program, weight, children }
    


parseProgram :: String -> Maybe String
parseProgram input =
    String.split (String.Pattern " ") input
    # (flip Array.index) 0


parseWeight :: String -> Maybe String
parseWeight input =
    String.split (String.Pattern " ") input
    # (flip Array.index) 1
    # map (\weight ->
        weight
        # String.drop 1
        # String.takeWhile (_ /= ')')
    )


parseChildren :: String -> Array String
parseChildren input =
    String.split (String.Pattern ", ") input


test = do
    let 
        puzzle = """pbga (66)
xhth (57)
ebii (61)
havc (66)
ktlj (57)
fwft (72) -> ktlj, cntj, xhth
qoyq (66)
padx (45) -> pbga, havc, qoyq
tknk (41) -> ugml, padx, fwft
jptl (61)
ugml (68) -> gyxo, ebii, jptl
gyxo (61)
cntj (57)"""

    print puzzle "tknk"
    pure unit
