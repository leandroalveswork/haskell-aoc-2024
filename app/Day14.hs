module Day14 (testAll, displayMoves) where
--module Day14 (testAll, answer, displayMoves) where

import qualified Data.List as DL
import qualified Data.List.Split as DLS

import Debugging (DWriter(DWriter), debug, getWritten, tell)
import TestLib (Comparation(Comparation))
import DataMassParsers (getFileLines)

newtype Vec = Vec (Int, Int)

multiplyVec :: Int -> Vec -> Vec
multiplyVec n (Vec (x, y)) = Vec (x * n, y * n)

extraordAdd :: (Int, Int) -> Vec -> Vec -> Vec
extraordAdd (xlen, ylen) (Vec (vx, vy)) (Vec (vx', vy')) = Vec ((vx + vx') `mod` xlen, (vy + vy') `mod` ylen)

moveRobots :: (Int, Int) -> Int -> [(Vec, Vec)] -> [(Vec, Vec)]
moveRobots (xlen, ylen) n = map (\(p, speed) -> (extraordAdd (xlen, ylen) p (multiplyVec n speed), speed))

data Quadrant = TopLeft | TopRight | BotLeft | BotRight
    deriving Eq

getQuadrantScore :: (Int, Int) -> [(Vec, Vec)] -> Quadrant -> Int
getQuadrantScore (xlen, ylen) robots TopLeft  = length (filter ((\(Vec (x, y)) -> x < (xlen `div` 2) && y < (ylen `div` 2)) . fst) robots)
getQuadrantScore (xlen, ylen) robots TopRight = length (filter ((\(Vec (x, y)) -> x > (xlen `div` 2) && y < (ylen `div` 2)) . fst) robots)
getQuadrantScore (xlen, ylen) robots BotLeft  = length (filter ((\(Vec (x, y)) -> x < (xlen `div` 2) && y > (ylen `div` 2)) . fst) robots)
getQuadrantScore (xlen, ylen) robots BotRight = length (filter ((\(Vec (x, y)) -> x > (xlen `div` 2) && y > (ylen `div` 2)) . fst) robots)

puzz1 :: [String]
puzz1 =
    [ "p=0,4 v=3,-3"
    , "p=6,3 v=-1,-3"
    , "p=10,3 v=-1,2"
    , "p=2,0 v=2,-1"
    , "p=0,0 v=1,3"
    , "p=3,0 v=-2,-2"
    , "p=7,6 v=-1,-3"
    , "p=3,0 v=-1,-2"
    , "p=9,3 v=2,3"
    , "p=7,3 v=-1,2"
    , "p=2,4 v=2,-3"
    , "p=9,5 v=-3,-3"
    ]

fromPuzzle :: [String] -> [(Vec, Vec)]
fromPuzzle = map (\s -> let positS = head (DLS.splitOn " " s)
                            speedS = (DLS.splitOn " " s) !! 1
                        in (toVec (drop 2 positS), toVec (drop 2 speedS))
                 )
    where toVec :: String -> Vec
          toVec st = let x = read (head (DLS.splitOn "," st))
                         y = read ((DLS.splitOn "," st) !! 1)
                     in Vec (x, y)

quadrantScoreComps :: [Comparation Int]
quadrantScoreComps =
    [ Comparation (101, 1, getQuadrantScore (11, 7) ((moveRobots (11, 7) 100 . fromPuzzle) puzz1) TopLeft)
    , Comparation (102, 3, getQuadrantScore (11, 7) ((moveRobots (11, 7) 100 . fromPuzzle) puzz1) TopRight)
    , Comparation (103, 4, getQuadrantScore (11, 7) ((moveRobots (11, 7) 100 . fromPuzzle) puzz1) BotLeft)
    , Comparation (104, 1, getQuadrantScore (11, 7) ((moveRobots (11, 7) 100 . fromPuzzle) puzz1) BotRight)
    ]

testAll :: IO ()
testAll = do
    (putStrLn . unlines . map show) quadrantScoreComps

answer :: IO ()
answer = do
    mass <- getFileLines "day-14.txt"
    print $ getProductOfQuadrants $ moveRobots (101, 103) 100 $ (fromPuzzle . filter ((>=1).length)) mass
    where getProductOfQuadrants :: [(Vec, Vec)] -> Integer
          getProductOfQuadrants robots = let tl = getQuadrantScore (101, 103) robots TopLeft
                                             tr = getQuadrantScore (101, 103) robots TopRight
                                             bl = getQuadrantScore (101, 103) robots BotLeft
                                             br = getQuadrantScore (101, 103) robots BotRight
                                         in (toInteger tl) * (toInteger tr) * (toInteger bl) * (toInteger br)

showRobots :: (Int, Int) -> [(Vec, Vec)] -> String
showRobots (xlen, ylen) robots = unlines (map (\y -> concatMap (\x -> printQuantity x y) [0..(xlen - 1)]) [0..(ylen - 1)])
    where printQuantity :: Int -> Int -> String
          printQuantity x' y' = let digit :: String
                                    digit = (show . min 9 . length . filter (\(Vec (vx, vy), _) -> vx == x' && vy == y')) robots
                                in if digit == "0" then "." else digit

-- 31, 72 and 134 = hot hint
displayMoves :: IO ()
displayMoves = do
    mass <- getFileLines "day-14.txt"
    (let parsedMass :: [(Vec, Vec)]
         parsedMass = (fromPuzzle . filter ((>=1).length)) mass
     in foldl
            (\m nTimes -> m >> putStrLn ("\nPICTURE " ++ show nTimes ++ "\n") >> putStrLn (showRobots (101, 103) (moveRobots (101, 103) nTimes parsedMass)))
            (return ())
            (map (\n -> 31 + 103 * n) [71..75])) -- PICTURE 7344
