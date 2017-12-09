module Day9 where

import Data.Maybe
import Prelude

import Control.Monad.Eff.Console (log)
import Control.MonadZero (guard)
import Data.Array as Array
import Data.Int as Int
import Data.Map (Map(..))
import Data.Map as Map
import Data.String as String
import Data.Traversable (traverse)
-- import Debug.Trace as Debug
import Node.Encoding as Encoding
import Node.FS.Sync as FS


data Token
    = GroupOpen
    | GroupClose
    | GarbageOpen
    | GarbageClose
    | IgnoreNext
    | AnyChar


data Stream a
    = Group a
    | Garbage


tokenize :: String -> Array Token
tokenize input =
    let
        chars =
            String.toCharArray input
    in
    runTokenizer chars []


runTokenizer :: Array Char -> Array Token -> Array Token
runTokenizer input tokens =
    case Array.uncons input of
        Just { head, tail } ->
            Array.snoc tokens (parseToken head)
            # runTokenizer tail
        Nothing ->
            tokens


parseToken :: Char -> Token
parseToken char =
    case char of
        '{' -> GroupOpen
        '}' -> GroupClose
        '<' -> GarbageOpen
        '>' -> GarbageClose
        '!' -> IgnoreNext
        _   -> AnyChar


solution _ = do
    input <- FS.readTextFile Encoding.UTF8 "input/day9.txt"
    let { score } = solve input
    pure $ show score


solve :: String -> { groups :: Int, score :: Int }
solve input =
    { groups: 1, score: 10 }


testGarbage input = do
    log "Garbage OK"


print input groups score = do
    let dump result =
            "(groups: " <> (show result.groups) <> ", score: " <> (show result.score) <> ")"

    log input
    log (dump $ solve input)
    log (dump { groups, score })
    log ""


test = do
    testGarbage "<>"
    testGarbage "<random characters>"
    testGarbage "<<<<>"
    testGarbage "<{!>}>"
    testGarbage "<!!>"
    testGarbage "<!!!>>"
    testGarbage """<{o"i!a,<{i<a>"""

    log "\n"

    -- print "{}" 1 1
    -- print "{{{}}}" 3 6
    -- print "{{},{}}" 3 5
    -- print "{{{},{},{{}}}}" 6 16
    -- print "{<{},{},{{}}>}" 1 1
    -- print "{<a>,<a>,<a>,<a>}" 1 1
    -- print "{{<a>},{<a>},{<a>},{<a>}}" 5 9
    -- print "{{<!>},{<!>},{<!>},{<a>}}" 2 3
    pure unit




    
