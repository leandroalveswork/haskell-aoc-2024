module Day01 (testAll) where

import qualified Data.List as DL

import TestLib (Comparation(Comparation))

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
