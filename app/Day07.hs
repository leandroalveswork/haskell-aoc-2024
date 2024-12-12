module Day07 (testAll, testAll2) where
--module Day07 (testAll, answer, testAll2, answer2) where

import qualified Data.List as DL
import qualified Data.List.Split as DLS

import TestLib (Comparation(Comparation))
import DataMassParsers (getFileLines)

possibleResults :: [Integer] -> [Integer]
possibleResults []  = []
possibleResults [x] = [x]
possibleResults xs' = let (x:xs) = reverse xs'
                      in map (*x) (possibleResults (reverse xs)) ++ map (+x) (possibleResults (reverse xs))

isPossible :: (Integer, [Integer]) -> Bool
isPossible (n, xs) = n `elem` possibleResults xs

isPossibleComps :: [Comparation Bool]
isPossibleComps =
    [ Comparation (1, True, isPossible (190, [10, 19]))
    , Comparation (2, True, isPossible (3267, [81, 40, 27]))
    , Comparation (3, False, isPossible (83, [11, 6, 16, 20]))
    , Comparation (4, False, isPossible (156, [15, 6]))
    , Comparation (5, False, isPossible (7290, [6, 8, 6, 15]))
    , Comparation (6, False, isPossible (161011, [16, 10, 13]))
    , Comparation (7, True, isPossible (292, [11, 6, 16, 20]))
    ]

testAll :: IO ()
testAll = (putStrLn . unlines . map show) isPossibleComps

{-
answer :: IO ()
answer = do
    mass <- getFileLines "day-07.txt"
    print $ sum $ map fst $ filter isPossible $ fromMass mass

fromMass :: [String] -> [(Integer, [Integer])]
fromMass = map (
        ( \xs -> (read (head xs), map read (DLS.splitOn " " (xs !! 1)) ) )
            . filter ((>=1).length)
            . DLS.splitOn ": "
    )
    . filter ((>=1).length)
    -}

possibleResultsWithConcat :: [Integer] -> [Integer]
possibleResultsWithConcat []  = []
possibleResultsWithConcat [x] = [x]
possibleResultsWithConcat xs' = let (x:xs) = reverse xs'
                                in map (*x) (possibleResultsWithConcat (reverse xs))
                                    ++ map (+x) (possibleResultsWithConcat (reverse xs))
                                    ++ map (\n -> read (show n ++ show x)) (possibleResultsWithConcat (reverse xs))

isPossibleWithConcat :: (Integer, [Integer]) -> Bool
isPossibleWithConcat (n, xs) = n `elem` possibleResultsWithConcat xs

concatComps :: [Comparation Bool]
concatComps =
    [ Comparation (101, True, isPossibleWithConcat (190, [10, 19]))
    , Comparation (102, True, isPossibleWithConcat (3267, [81, 40, 27]))
    , Comparation (103, False, isPossibleWithConcat (83, [11, 6, 16, 20]))
    , Comparation (104, True, isPossibleWithConcat (156, [15, 6]))
    , Comparation (105, True, isPossibleWithConcat (7290, [6, 8, 6, 15]))
    , Comparation (106, False, isPossibleWithConcat (161011, [16, 10, 13]))
    , Comparation (107, True, isPossibleWithConcat (292, [11, 6, 16, 20]))
    ]

testAll2 :: IO ()
testAll2 = (putStrLn . unlines . map show) concatComps

{-
answer2 :: IO ()
answer2 = do
    mass <- getFileLines "day-07.txt"
    print $ sum $ map fst $ filter isPossibleWithConcat $ fromMass mass
    -}
