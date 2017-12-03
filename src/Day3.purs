module Day3 where

import Data.Maybe
import Prelude

import Control.Monad.Eff.Console (log)
import Data.Array as Array
import Data.Map (Map(..))
import Data.Map as Map
import Data.Tuple (Tuple(..))
import Data.Ord (abs)
-- import Debug.Trace as Debug


data Point =
    Point { x :: Int, y :: Int }

derive instance eqPoint  :: Eq  Point
derive instance ordPoint :: Ord Point

instance showPoint :: Show Point where
    show (Point {x, y}) = "x: " <> (show x) <> ", y: " <> (show y)

type Matrix =
    Map Point Int

solution _ = do
    let target = 368078
    let params = { point: Point {x: 1, y: -1}, current: 9, target }
    let matrix = buildMatrix params initial
    let Point { x, y } = position matrix target

    pure (abs x + abs y)

type Square a =
    {
        topLeft :: a,
        top :: a,
        topRight :: a,
        right :: a,
        bottomRight :: a,
        bottom :: a,
        bottomLeft :: a,
        left :: a,
        center :: a
    }

data Position
    = TopLeft
    | Top
    | TopRight
    | Right
    | BottomRight
    | Bottom
    | BottomLeft
    | Left
    | Center


derive instance eqPosition  :: Eq  Position
derive instance ordPosition :: Ord Position


initial =
    let
        center =
            Point { x: 0, y: 0 }

        add pos n =
            Map.insert (pointFromPosition center pos) n
    in
    Map.empty
    # add Center      1
    # add Right       2
    # add TopRight    3
    # add Top         4
    # add TopLeft     5
    # add Left        6
    # add BottomLeft  7
    # add Bottom      8
    # add BottomRight 9


buildMatrix
    :: { point :: Point, current :: Int, target :: Int }
    -> Matrix
    -> Matrix
buildMatrix { point, current, target } matrix
    | current == target = matrix
    | otherwise =
        let
            square =
                buildSquare matrix point
            next   =
                nextPosition square
                # pointFromPosition point
            newMatrix =
                Map.insert next (current + 1) matrix
        in
        buildMatrix { point: next, current: current + 1, target } newMatrix

position :: Matrix -> Int -> Point
position matrix n =
    let
        arr =
            Map.toUnfoldable matrix :: Array (Tuple Point Int)
    in
    arr
    # Array.find (\(Tuple _ v) -> v == n)
    # maybe (Point { x: 0, y: 0 }) (\(Tuple k _) -> k)


pointFromPosition :: Point -> Position -> Point
pointFromPosition (Point { x, y }) TopLeft     = Point { x: x - 1, y: y + 1 }
pointFromPosition (Point { x, y }) Top         = Point { x: x,     y: y + 1 }
pointFromPosition (Point { x, y }) TopRight    = Point { x: x + 1, y: y + 1 }
pointFromPosition (Point { x, y }) Right       = Point { x: x + 1, y: y     }
pointFromPosition (Point { x, y }) BottomRight = Point { x: x + 1, y: y - 1 }
pointFromPosition (Point { x, y }) Bottom      = Point { x: x,     y: y - 1 }
pointFromPosition (Point { x, y }) BottomLeft  = Point { x: x - 1, y: y - 1 }
pointFromPosition (Point { x, y }) Left        = Point { x: x - 1, y: y     }
pointFromPosition (Point { x, y }) Center      = Point { x, y }



-- computes the square of positions surrounding the given point
buildSquare :: Matrix -> Point -> Square Boolean
buildSquare matrix point =
    let
        add pos =
            Map.member (pointFromPosition point pos) matrix
            # Map.insert pos
    in
    Map.empty
    # add TopLeft
    # add Top
    # add TopRight
    # add Right
    # add BottomRight
    # add Bottom
    # add BottomLeft
    # add Left
    # dictToSquare


dictToSquare :: Map Position Boolean -> Square Boolean
dictToSquare dict =
    let
        lookup pos =
            Map.lookup pos dict
            # maybe false id
    in
    {
        topLeft: lookup TopLeft,
        top: lookup Top,
        topRight: lookup TopRight,
        right: lookup Right,
        bottomRight: lookup BottomRight,
        bottom: lookup Bottom,
        bottomLeft: lookup BottomLeft,
        left: lookup Left,
        center: lookup Center
    }


nextPosition :: Square Boolean -> Position
nextPosition square =
    let
        {
            topLeft,
            top,
            topRight,
            right,
            bottomRight,
            bottom,
            bottomLeft,
            left,
            center
        } = square
    in
    if top && topLeft && left then Right
    else if topLeft && left then Top
    else if left && bottom && bottomLeft then Top
    else if bottom && bottomLeft then Left
    else if bottom && right && bottomRight then Left
    else if right && bottomRight then Bottom
    else if top && topRight && right then Bottom
    else if top && topRight then Right
    else Center

test = do
    let params = { point: Point {x: 1, y: -1}, current: 9, target: 23 }
    let matrix = buildMatrix params initial
    log $ (show $ position matrix 23)
    log "should be equal to 0, -2"
    pure unit
