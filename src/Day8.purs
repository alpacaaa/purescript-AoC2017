module Day8 where

import Data.Maybe
import Prelude

import Control.Monad.Eff.Console (log)
import Control.MonadZero (guard)
import Data.Array as Array
import Data.Int as Int
import Data.List as List
import Data.Map (Map(..))
import Data.Map as Map
import Data.String as String
import Data.Traversable (traverse)
import Debug.Trace as Debug
import Node.Encoding as Encoding
import Node.FS.Sync as FS


data Operation
    = Increase Int
    | Decrease Int

data Sign
    = Equal
    | NotEqual
    | Less
    | LessOrEqual
    | Greater
    | GreaterOrEqual

data Condition
    = Condition String Sign Int

type Instruction =
    { register  :: String
    , operation :: Operation
    , condition :: Condition
    }


verifyCondition :: Int -> Condition -> Boolean
verifyCondition a (Condition _ Equal b)          = a == b
verifyCondition a (Condition _ NotEqual b)       = a /= b
verifyCondition a (Condition _ Less b)           = a <  b
verifyCondition a (Condition _ LessOrEqual b)    = a <= b
verifyCondition a (Condition _ Greater b)        = a >  b
verifyCondition a (Condition _ GreaterOrEqual b) = a >= b


applyOperation :: Operation -> Int -> Int
applyOperation (Increase amount) n = n + amount
applyOperation (Decrease amount) n = n - amount


parseOperation :: String -> Int -> Maybe Operation
parseOperation op amount =
    case op of
        "inc" -> Just (Increase amount)
        "dec" -> Just (Decrease amount)
        _     -> Nothing


parseSign :: String -> Maybe Sign
parseSign sign =
    case sign of
        "==" -> Just Equal
        "!=" -> Just NotEqual
        "<"  -> Just Less
        "<=" -> Just LessOrEqual
        ">"  -> Just Greater
        ">=" -> Just GreaterOrEqual
        _    -> Nothing


parseCondition :: String -> String -> Int -> Maybe Condition
parseCondition register sign' amount =
    parseSign sign'
    # map \sign -> Condition register sign amount


parseInstruction :: String -> Maybe Instruction
parseInstruction input = do
    let
        parts =
            String.split (String.Pattern " ") input

    guard (Array.length parts /= 7)

    register <- Array.index parts 0
    op       <- Array.index parts 1
    amount   <- Array.index parts 2 >>= Int.fromString
    cond     <- Array.index parts 3
    otherReg <- Array.index parts 4
    sign     <- Array.index parts 5
    target   <- Array.index parts 6 >>= Int.fromString

    operation <- parseOperation op amount
    condition <- parseCondition otherReg sign target

    pure { register, operation, condition }


solution _ = do
    pure 55
    -- input <- FS.readTextFile Encoding.UTF8 "input/day8.txt"
    -- pure $ maybe "it's broken" id (solve input)


print input expected = do
    log $ (show $ solve input) <> " expecting: " <> show expected


solve :: String -> Maybe Int
solve input =
    Just 11



test = do
    let 
        puzzle = """b inc 5 if a > 1
a inc 1 if b < 5
c dec -10 if a >= 1
c inc -20 if c == 10"""

    print puzzle 1
    pure unit
