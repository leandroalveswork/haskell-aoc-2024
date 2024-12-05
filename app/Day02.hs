module Day02 (testAll) where
--module Day02 (testAll, answer) where

import qualified Data.Maybe as DM
import qualified Data.List.Split as DLS

import TestLib (Comparation(Comparation))
import DataMassParsers (getFileLines)

data Report = Nil | Cons (Int, Report)

data Direction = Increasing | Decreasing

build :: Direction -> [Int] -> Maybe Report
build dir []     = Just Nil
build dir (x:xs) = do
    r <- build dir xs
    case r of Nil         -> Just (Cons (x, r))
              Cons (y, _) -> if changesSafely dir x y
                                then Just (Cons (x, r))
                                else Nothing
    where changesSafely :: Direction -> Int -> Int -> Bool
          changesSafely Increasing x' y' = (x' - y') `elem` [1, 2, 3]
          changesSafely Decreasing x' y' = (x' - y') `elem` [-1, -2, -3]

buildFromBothWays :: [Int] -> Maybe Report
buildFromBothWays xs = case build Decreasing xs of
                            Nothing -> build Increasing xs
                            Just y  -> Just y

comps :: [Comparation Bool]
comps =
    [ Comparation (1, True, DM.isJust (buildFromBothWays [7, 6, 4, 2, 1]))
    , Comparation (2, False, DM.isJust (buildFromBothWays [1, 2, 7, 8, 9]))
    , Comparation (3, False, DM.isJust (buildFromBothWays [9, 7, 6, 2, 1]))
    , Comparation (4, False, DM.isJust (buildFromBothWays [1, 3, 2, 4, 5]))
    , Comparation (5, False, DM.isJust (buildFromBothWays [8, 6, 4, 4, 1]))
    , Comparation (6, True, DM.isJust (buildFromBothWays [1, 3, 6, 7, 9]))
    ]

testAll :: IO ()
testAll = (putStrLn . unlines . map show) comps

    {-

countSafeReports :: [[Int]] -> Int
countSafeReports = length . filter DM.isJust . map buildFromBothWays

answer :: IO ()
answer = do
    mass <- getFileLines "day-02.txt"
    print $ countSafeReports $ fromMass mass

fromMass :: [String] -> [[Int]]
fromMass = map (
        map read . filter ((>=1).length) . DLS.splitOn " "
    )
    . filter ((>=1).length)

    -}
