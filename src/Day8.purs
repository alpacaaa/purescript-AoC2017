module Day8 where

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

type Result =
    Map String Int


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


applyInstruction :: Result -> Instruction -> Result
applyInstruction result instruction =
    let
        register =
            lookupValue instruction.register result

        newValue =
            applyOperation instruction.operation register 
    in
    Map.insert instruction.register newValue result


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

    guard (Array.length parts == 7)

    register <- Array.index parts 0
    op       <- Array.index parts 1
    amount   <- Array.index parts 2 >>= Int.fromString
    otherReg <- Array.index parts 4
    sign     <- Array.index parts 5
    target   <- Array.index parts 6 >>= Int.fromString


    operation <- parseOperation op amount
    condition <- parseCondition otherReg sign target

    pure { register, operation, condition }


solution _ = do
    input <- FS.readTextFile Encoding.UTF8 "input/day8.txt"
    pure $ maybe "it's broken" show (solve input)


print input expected = do
    log $ (show $ solve input) <> " expecting: " <> show expected


solve :: String -> Maybe Int
solve input =
    input
    # String.split (String.Pattern "\n")
    # traverse parseInstruction
    # map (Array.foldl solve' Map.empty)
    # map findResut


lookupValue :: String -> Result -> Int
lookupValue register result =
    Map.lookup register result
    # maybe 0 id -- Registers default to zero


solve' :: Result -> Instruction -> Result
solve' result instruction =
    let
        value =
            lookupValue (conditionRegister instruction) result
    in
    if verifyCondition value instruction.condition
        then applyInstruction result instruction
        else result


findResut :: Result -> Int
findResut result =
    Array.foldl max 0 result


conditionRegister :: Instruction -> String
conditionRegister instruction =
    let
        (Condition register _ _) = instruction.condition
    in
    register

test = do
    let 
        puzzle = """b inc 5 if a > 1
a inc 1 if b < 5
c dec -10 if a >= 1
c inc -20 if c == 10"""

    print puzzle 1
    pure unit
