module Day01 (testAll, testAll2) where
--module Day01 (testAll, answer, testAll2, answer2) where

import qualified Data.List as DL
import qualified Data.List.Split as DLS

import TestLib (Comparation(Comparation))
import DataMassParsers (getFileLines)

sortForSum :: [(Int, Int)] -> [(Int, Int)]
sortForSum xys = let xs = DL.sort (map fst xys)
                     ys = DL.sort (map snd xys)
                 in zip xs ys

sortComps :: [Comparation [(Int, Int)]]
sortComps =
    [ Comparation (1, [], sortForSum [])
    , Comparation (2, [(1, 2), (3, 5), (10, 11)], sortForSum [(1, 5), (10, 2), (3, 11)])
    , Comparation (3, [(1, 5)], sortForSum [(1, 5)])
    , Comparation (4, [(5, 1)], sortForSum [(5, 1)])
    , Comparation (5, [(9, 9)], sortForSum [(9, 9)])
    , Comparation (6, [(8, 8), (9, 9), (10, 10)], sortForSum [(8, 10), (9, 9), (10, 8)])
    , Comparation (7, [(1, 0), (1, 11), (3, 20), (4, 21), (100, 54)], sortForSum [(1, 54), (4, 21), (3, 11), (1, 20), (100, 0)])
    ]

sumDistances :: [(Int, Int)] -> Int
sumDistances = sum . map (abs . uncurry (-)) . sortForSum

sumComps :: [Comparation Int]
sumComps =
    [ Comparation (11, 0, sumDistances [])
    , Comparation (12, 4, sumDistances [(1, 5), (10, 2), (3, 11)])
    , Comparation (13, 4, sumDistances [(1, 5)])
    , Comparation (14, 4, sumDistances [(5, 1)])
    , Comparation (15, 0, sumDistances [(9, 9)])
    , Comparation (16, 0, sumDistances [(8, 10), (9, 9), (10, 8)])
    , Comparation (17, 91, sumDistances [(1, 54), (4, 21), (3, 11), (1, 20), (100, 0)])
    ]

testAll :: IO ()
testAll = do
    (putStrLn . unlines . map show) sortComps
    (putStrLn . unlines . map show) sumComps

    {-
answer :: IO ()
answer = do
    mass <- getFileLines "day-01.txt"
    print $ sumDistances $ fromMass mass
fromMass :: [String] -> [(Int, Int)]
fromMass = map (
        ( \xs -> (read (head xs), read (xs !! 1)) )
            . filter ((>=1).length)
            . DLS.splitOn " "
    )
    . filter ((>=1).length)
    -}

occurrences :: Int -> [Int] -> Int
occurrences n = length . filter (==n)

similarity :: [(Int, Int)] -> Int
similarity xys = let xs = map fst xys
                     ys = map snd xys
                 in sum (map (\n -> n * occurrences n ys) xs)

simComps :: [Comparation Int]
simComps =
    [ Comparation (101, 0, similarity [(1, 5)])
    , Comparation (102, 2, similarity [(2, 2)])
    , Comparation (103, 31, similarity [(3, 4), (4, 3), (2, 5), (1, 3), (3, 9), (3, 3)])
    ]

testAll2 :: IO ()
testAll2 = (putStrLn . unlines . map show) simComps

    {-
answer2 :: IO ()
answer2 = do
    mass <- getFileLines "day-01.txt"
    print $ similarity $ fromMass mass
    -}
