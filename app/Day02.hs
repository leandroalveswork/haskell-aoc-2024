module Day02 (testAll, testAll2) where
--module Day02 (testAll, answer, testAll2, answer2) where

import qualified Data.Maybe as DM
import qualified Data.Foldable as DF
import qualified Data.List.Split as DLS

import TestLib (Comparation(Comparation))
import DataMassParsers (getFileLines)

data Report = Nil | Cons (Int, Report)

data Direction = Increasing | Decreasing

changesSafely :: Direction -> Int -> Int -> Bool
changesSafely Increasing x' y' = (x' - y') `elem` [1, 2, 3]
changesSafely Decreasing x' y' = (x' - y') `elem` [-1, -2, -3]

build :: Direction -> [Int] -> Maybe Report
build dir []     = Just Nil
build dir (x:xs) = do
    r <- build dir xs
    case r of Nil         -> Just (Cons (x, r))
              Cons (y, _) -> if changesSafely dir x y
                                then Just (Cons (x, r))
                                else Nothing

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

buildWithCheckpoint :: Direction -> [Int] -> Maybe Report
buildWithCheckpoint dir xs =
    case conflictRightIndices xs of
         []   -> build dir xs
         many -> DF.foldl
                    (\acc astk -> mappendMaybes (mappendMaybes
                        (build dir (filterIndex astk xs))
                        (build dir (filterIndex (astk - 1) xs))
                        ) acc
                    )
                    Nothing
                    many
    where conflictRightIndices :: [Int] -> [Int]
          conflictRightIndices xs = map (\(t1, t2, t3) -> t1)
              $ filter (\(t1, t2, t3) -> not (changesSafely dir t2 t3))
              $ drop 1
              $ zip3 [1..] (0:xs) xs
          mappendMaybes :: Maybe a -> Maybe a -> Maybe a
          mappendMaybes (Just i) (Just j) = Just i
          mappendMaybes (Just i) Nothing  = Just i
          mappendMaybes Nothing  (Just j) = Just j
          mappendMaybes Nothing  Nothing  = Nothing
          filterIndex :: Int -> [a] -> [a]
          filterIndex n = map snd . filter (\(o, p) -> o /= n) . zip [1..]

buildWithCheckpointFromBothWays :: [Int] -> Maybe Report
buildWithCheckpointFromBothWays xs = case buildWithCheckpoint Decreasing xs of
                                          Nothing -> buildWithCheckpoint Increasing xs
                                          Just y  -> Just y

checkpointComps :: [Comparation Bool]
checkpointComps =
    [ Comparation (101, True, DM.isJust (buildWithCheckpointFromBothWays [7, 6, 4, 2, 1]))
    , Comparation (102, False, DM.isJust (buildWithCheckpointFromBothWays [1, 2, 7, 8, 9]))
    , Comparation (103, False, DM.isJust (buildWithCheckpointFromBothWays [9, 7, 6, 2, 1]))
    , Comparation (104, True, DM.isJust (buildWithCheckpointFromBothWays [1, 3, 2, 4, 5]))
    , Comparation (105, True, DM.isJust (buildWithCheckpointFromBothWays [8, 6, 4, 4, 1]))
    , Comparation (106, True, DM.isJust (buildWithCheckpointFromBothWays [1, 3, 6, 7, 9]))
    , Comparation (107, False, DM.isJust (buildWithCheckpointFromBothWays [16, 13, 16, 16, 17, 19, 21, 19]))
    , Comparation (108, False, DM.isJust (buildWithCheckpointFromBothWays [88, 85, 92, 93, 94]))
    , Comparation (109, True, DM.isJust (buildWithCheckpointFromBothWays [90, 6, 5, 4, 1]))
    , Comparation (110, False, DM.isJust (buildWithCheckpointFromBothWays [90, 6, 4, 4, 1]))
    , Comparation (111, True, DM.isJust (buildWithCheckpointFromBothWays [7, 6, 5, 4, 89]))
    , Comparation (112, False, DM.isJust (buildWithCheckpointFromBothWays [1, 3, 6, 7, 89, 9, 10, 12, 120, 13]))
    ]

testAll2 :: IO ()
testAll2 = (putStrLn . unlines . map show) checkpointComps

    {-
countSafeReportsWithCheckpoint :: [[Int]] -> Int
countSafeReportsWithCheckpoint = length . filter DM.isJust . map buildWithCheckpointFromBothWays

answer2 :: IO ()
answer2 = do
    mass <- getFileLines "day-02.txt"
    print $ countSafeReportsWithCheckpoint $ fromMass mass
    -}
