module Day08 (testAll, testAll2) where
--module Day08 (testAll, answer, testAll2, answer2) where

import qualified Data.List as DL
import qualified Data.List.Split as DLS

import TestLib (Comparation(Comparation))
import DataMassParsers (getFileLines)

-- antinodes de 5,6 e 9,10 = 1,2 + 13,14
-- varx = 4, vary = 4
pairAntinodes :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
pairAntinodes (x, y) (x', y') = let varx = x' - x
                                    vary = y' - y
                                in [(x' + varx, y' + vary), (x - varx, y - vary)]

getAntinodes :: ([(Int, Int, Char)], (Int, Int)) -> [(Int, Int)]
getAntinodes (cs_, bounds_) = DL.nub (getAntinodesByMyself (DL.sortOn (\(_, _, x) -> x) cs_, bounds_))
    where getAntinodesByMyself (cs', (maxx, maxy)) = case cs' of
                                                          []     -> []
                                                          (c:cs) -> let (_, _, letC) = c
                                                                        taken = map (\(x, y, _) -> (x, y)) (takeWhile (\(_, _, x) -> x == letC) (c:cs))
                                                                        next = dropWhile (\(_, _, x) -> x == letC) (c:cs)
                                                                        allCombs = (,) <$> [0..(length taken - 1)] <*> [0..(length taken - 1)]
                                                                        diffCombs = filter (uncurry (/=)) allCombs
                                                                        rawAntins = DL.nub (concatMap (\(i, j) -> pairAntinodes (taken !! i) (taken !! j)) diffCombs)
                                                                    in filter (\(x, y) -> x >= 0 && x <= maxx && y >= 0 && y <= maxy) rawAntins ++ getAntinodesByMyself (next, (maxx, maxy))

fromLines :: [String] -> ([(Int, Int, Char)], (Int, Int))
fromLines lins = let zippedPoses = concat (zipWith
                                                (\j1 line -> zipWith (\i1 e -> (i1, j1, e) ) [0..] line)
                                                [0..]
                                                lins)
                     maxx = maximum $ map (\(x, y, c) -> x) zippedPoses
                     maxy = maximum $ map (\(x, y, c) -> y) zippedPoses
                     validChars = filter (\(x, y, c) -> c `elem` ['0'..'9'] || c `elem` ['A'..'Z'] || c `elem` ['a'..'z']) zippedPoses
                 in (validChars, (maxx, maxy))

lns1 :: [String]
lns1 =
    [ "......#....#"
    , "...#....0..."
    , "....#0....#."
    , "..#....0...."
    , "....0....#.."
    , ".#....A....."
    , "...#........"
    , "#......#...."
    , "........A..."
    , ".........A.."
    , "..........#."
    , "..........#."
    ]

getAntinodesComps :: [Comparation Int]
getAntinodesComps =
    [ Comparation (1, 14, (length . getAntinodes . fromLines) lns1)
    ]

testAll :: IO ()
testAll = (putStrLn . unlines . map show) getAntinodesComps

{-
answer :: IO ()
answer = do
    mass <- getFileLines "day-08.txt"
    (print . length . getAntinodes . fromLines) mass
    -}

inlineAntinodes :: (Int, Int) -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
inlineAntinodes (x, y) (x', y') (maxx, maxy) = let varx = x' - x
                                                   vary = y' - y
                                                   mapForward :: [Int] -> [(Int, Int)]
                                                   mapForward = map (\mut -> (x' + varx * mut, y' + vary * mut))
                                                   mapBackward :: [Int] -> [(Int, Int)]
                                                   mapBackward = map (\mut -> (x - varx * mut, y - vary * mut))
                                                   filterByBounds :: [(Int, Int)] -> [(Int, Int)]
                                                   filterByBounds = filter (\(x_, y_) -> x_ >= 0 && x_ <= maxx && y_ >= 0 && y_ <= maxy)
                                                   lengthToTakeExact :: (Int, Int)
                                                   lengthToTakeExact = let (xQuoc, xRemaind) = x `divMod` abs varx
                                                                           (totalQuoc, totalRemaind) = maxx `divMod` abs varx
                                                                       in (xQuoc, totalQuoc - xQuoc)
                                               in (filterByBounds . mapBackward) [0..(fst lengthToTakeExact)] ++ (filterByBounds . mapForward) [0..(snd lengthToTakeExact)]

getAntinodesInlined :: ([(Int, Int, Char)], (Int, Int)) -> [(Int, Int)]
getAntinodesInlined (cs_, bounds_) = DL.nub (getAntinodesByMyself (DL.sortOn (\(_, _, x) -> x) cs_, bounds_))
    where getAntinodesByMyself (cs', (maxx, maxy)) = case cs' of
                                                          []     -> []
                                                          (c:cs) -> let (_, _, letC) = c
                                                                        taken = map (\(x, y, _) -> (x, y)) (takeWhile (\(_, _, x) -> x == letC) (c:cs))
                                                                        next = dropWhile (\(_, _, x) -> x == letC) (c:cs)
                                                                        allCombs = (,) <$> [0..(length taken - 1)] <*> [0..(length taken - 1)]
                                                                        diffCombs = filter (uncurry (/=)) allCombs
                                                                        rawAntins = DL.nub (concatMap (\(i, j) -> inlineAntinodes (taken !! i) (taken !! j) (maxx, maxy)) diffCombs)
                                                                    in rawAntins ++ getAntinodesByMyself (next, (maxx, maxy))

lns2 :: [String]
lns2 =
    [ "T....#...."
    , "...T......"
    , ".T....#..."
    , ".........#"
    , "..#......."
    , ".........."
    , "...#......"
    , ".........."
    , "....#....."
    , ".........."
    ]

getInlineComps :: [Comparation Int]
getInlineComps =
    [ Comparation (101, 34, (length . getAntinodesInlined . fromLines) lns1)
    ,  Comparation (102, 9, (length . getAntinodesInlined . fromLines) lns2)
    ]

testAll2 :: IO ()
testAll2 = (putStrLn . unlines . map show) getInlineComps

{-
answer2 :: IO ()
answer2 = do
    mass <- getFileLines "day-08.txt"
    (print . length . getAntinodesInlined . fromLines) mass
    -}
