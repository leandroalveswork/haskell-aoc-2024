module Day04 (testAll, testAll2) where
--module Day04 (testAll, answer, testAll2, answer2) where

import qualified Data.List.Split as DLS
import qualified Data.Maybe as DM

import TestLib (Comparation(Comparation))
import DataMassParsers (getFileLines)

data Angle = N_45 | N0 | N45 | N90 | N135 | N180

clockwise45 :: [[a]]-> [[a]]
clockwise45 []       = []
clockwise45 [xs]     = map (:[]) xs
clockwise45 (xs:xss) = let next = clockwise45 xss
                       in case xs of
                               []     -> []:next
                               (j:js) -> [j]:zipWith putAfter next (map Just js ++ repeat Nothing)
      where putAfter :: [a] -> Maybe a -> [a]
            putAfter a Nothing = a
            putAfter a (Just b) = a ++ [b]

anticlockwise45 :: [[a]]-> [[a]]
anticlockwise45 []       = []
anticlockwise45 [xs]     = map (:[]) (reverse xs)
anticlockwise45 (xs:xss) = let next = anticlockwise45 xss
                           in case reverse xs of
                                   []     -> []:next
                                   (j:js) -> [j]:zipWith putBefore next (map Just js ++ repeat Nothing)
      where putBefore :: [a] -> Maybe a -> [a]
            putBefore a Nothing = a
            putBefore a (Just b) = b:a

clockwise90 :: [[a]] -> [[a]]
clockwise90 []       = []
clockwise90 [xs]     = map (:[]) xs
clockwise90 (xs:xss) = let next = clockwise90 xss
                       in zipWith (\a b -> a ++ [b]) next xs

clockwise180 :: [[a]] -> [[a]]
clockwise180 []       = []
clockwise180 [xs]     = [reverse xs]
clockwise180 (xs:xss) = let next = clockwise180 xss
                        in next ++ [reverse xs]

turn :: Angle -> [[a]] -> [[a]]
turn N_45 = anticlockwise45
turn N0 = id
turn N45 = clockwise45
turn N90 = clockwise90
turn N135 = clockwise45 . clockwise90
turn N180 = clockwise180

{-
1                     2
2 6  <--- 2 1  --->   5 1
5         5 6         6
 -}

angulationComps :: [Comparation [[Int]]]
angulationComps =
      [ Comparation (1, [[1], [2, 6], [5]], turn N_45 [[2, 1], [5, 6]])
      , Comparation (2, [[2], [5, 1], [6]], turn N45 [[2, 1], [5, 6]])
      , Comparation (3, [[5, 2], [6, 1]], turn N90 [[2, 1], [5, 6]])
      , Comparation (4, [[5], [6, 2], [1]], turn N135 [[2, 1], [5, 6]])
      , Comparation (5, [[6, 5], [1, 2]], turn N180 [[2, 1], [5, 6]])
      ]

occurrences :: [[Char]] -> Int
occurrences = sum . map ((\a -> a - 1) . length . DLS.splitOn "XMAS")

occurrencesInAnyAngle :: [[Char]] -> Int
occurrencesInAnyAngle xs = occurrences xs
      + occurrences (turn N_45 xs)
      + occurrences (turn N45 xs)
      + occurrences (turn N90 xs)
      + occurrences (turn N135 xs)
      + occurrences (turn N180 xs)
      + occurrences ((turn N45 . turn N180) xs)
      + occurrences ((turn N90 . turn N180) xs)

occurencesComps :: [Comparation Int]
occurencesComps =
      [ Comparation (11, 18, occurrencesInAnyAngle ["MMMSXXMASM", "MSAMXMSMSA", "AMXSXMAAMM", "MSAMASMSMX", "XMASAMXAMM",
            "XXAMMXXAMA", "SMSMSASXSS", "SAXAMASAAA", "MAMMMXMMMM", "MXMXAXMASX"])
      , Comparation (12, 10, occurrencesInAnyAngle ["MMMSXXMASM", "MSAMXMSMSA", "AMXSXMAAMM", "MSAMASMSMX", "XMASAMXAMM",
            "XXAMMXXAMA", "SMSMSASXSS", "SAXAMASAAA", "MAMMMXMMMM"])
      , Comparation (13, 6, occurrencesInAnyAngle ["XMAS", "XMAS", "XMAS", "XMAS"])
      ]

testAll :: IO ()
testAll = do
      (putStrLn . unlines . map show) angulationComps
      (putStrLn . unlines . map show) occurencesComps

      {-
answer :: IO ()
answer = do
    mass <- getFileLines "day-04.txt"
    print $ occurrencesInAnyAngle mass
    -}

xOccurredTimes :: [[Char]] -> Int
xOccurredTimes css = let indexedL = concat (zipWith
                                                (\j1 line -> zipWith (\i1 e -> (i1, j1, e) ) [0..] line)
                                                [0..]
                                                css)
                     in (length . filter (isTheScenario css)) indexedL
      where getItemAt :: [[Char]] -> Int -> Int -> Maybe Char
            getItemAt css' x y
                  | y < 0 || y >= length css' = Nothing
                  | otherwise = let lineAt = css' !! y
                                in if x < 0 || x >= length lineAt
                                      then Nothing
                                      else Just (lineAt !! x)

            isTheScenario :: [[Char]] -> (Int, Int, Char) -> Bool
            isTheScenario css' (x', y', i) =
                  DM.isJust
                    (do
                        topLeft <- getItemAt css' (x' - 1) (y' - 1)
                        topRight <- getItemAt css' (x' + 1) (y' - 1)
                        botLeft <- getItemAt css' (x' - 1) (y' + 1)
                        botRight <- getItemAt css' (x' + 1) (y' + 1)
                        if topLeft == 'M' && topRight == 'M' && i == 'A' && botLeft == 'S' && botRight == 'S'
                           then Just '-' else Nothing
                    )

xOccurredTimesInAnyAngle :: [[Char]] -> Int
xOccurredTimesInAnyAngle xs = xOccurredTimes xs
      + xOccurredTimes (turn N90 xs)
      + xOccurredTimes (turn N180 xs)
      + xOccurredTimes ((turn N90 . turn N180) xs)

xOccurredComps :: [Comparation Int]
xOccurredComps =
      [ Comparation (101, 9, xOccurredTimesInAnyAngle [".M.S......", "..A..MSMS.", ".M.S.MAA..", "..A.ASMSM.", ".M.S.M....", ".........."
            , "S.S.S.S.S.", ".A.A.A.A..", "M.M.M.M.M.", ".........."])
      , Comparation (102, 9, xOccurredTimesInAnyAngle ["AM.S.....A", "..A..MSMS.", "AM.S.MAA.A", "..A.ASMSM.", ".M.S.M....", ".........."
            , "S.S.S.S.S.", ".A.A.A.A..", "M.M.M.M.M.", "AAAAAAAAAA"])
      ]

testAll2 :: IO ()
testAll2 = (putStrLn . unlines . map show) xOccurredComps

      {-
answer2 :: IO ()
answer2 = do
    mass <- getFileLines "day-04.txt"
    print $ xOccurredTimesInAnyAngle mass
    -}
