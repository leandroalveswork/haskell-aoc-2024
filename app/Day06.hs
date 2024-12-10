module Day06 (testAll, testAll2) where
--module Day06 (testAll, answer, testAll2, answer2) where

import qualified Data.List.Split as DLS
import qualified Data.Maybe as DM
import qualified Data.List as DL

import TestLib (Comparation(Comparation))
import DataMassParsers (getFileLines)

newtype Point = Point (Int, Int)

data Direction = Up | Right | Down | Left

turn :: Direction -> Direction
turn Up    = Right
turn Right = Down
turn Down  = Left
turn Left  = Up

moveFwd :: Direction -> Point -> Point
moveFwd Up    (x,y) = (x    ,y - 1)
moveFwd Right (x,y) = (x + 1,y    )
moveFwd Down  (x,y) = (x    ,y + 1)
moveFwd Left  (x,y) = (x - 1,y    )

moveUntilOutside :: Point -> [[Point]] -> Point
moveUntilOutside star obstacs = let maxx = foldl max 0 (map fst obstacs)
