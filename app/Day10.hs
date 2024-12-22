module Day10 (testAll, testAll2) where
--module Day10 (testAll, answer, testAll2, answer2) where

import qualified Data.List as DL
import qualified Data.List.Split as DLS
import Debugging (DWriter(DWriter), debug, getWritten, tell)

import TestLib (Comparation(Comparation))
import DataMassParsers (getFileLines)

newtype Matrix a = Matrix [(Int, Int, a)]
    deriving Eq

instance Functor Matrix where
    fmap f (Matrix ps) = Matrix (map (\(x, y, p) -> (x, y, f p)) ps)

toMatrix :: [[a]] -> Matrix a
toMatrix lns = Matrix (concat (zipWith
                                    (\j1 line -> zipWith (\i1 e -> (i1, j1, e) ) [0..] line)
                                    [0..]
                                    lns))

newtype HikesList = HikesList [(Int, Int, Int)]
    deriving Eq

instance Show HikesList where
    show (HikesList hs) = show hs

runHikesL :: HikesList -> [(Int, Int, Int)]
runHikesL (HikesList hs) = hs

canMoveTo :: (Int, Int) -> (Int, Int) -> Bool
canMoveTo (x', y') (x, y)
    | x' == x   = abs (y' - y) == 1
    | y' == y   = abs (x' - x) == 1
    | otherwise = False

fromMatrix :: Matrix Int -> (Int, Int) -> HikesList
fromMatrix (Matrix ps) (x, y) = let foundPInMatrix :: Maybe (Int, Int, Int)
                                    foundPInMatrix = DL.find (\(xp, yp, pp) -> xp == x && yp == y) ps
                                in case foundPInMatrix of
                                        Nothing          -> HikesList []
                                        Just (_, _, okP) -> HikesList (
                                                                filter
                                                                    (\(xp, yp, pp) -> canMoveTo (x, y) (xp, yp) && pp == okP + 1)
                                                                    ps
                                                                )

rawMat1 :: [String]
rawMat1 =
    [ "5550555"
    , "5551555"
    , "5552555"
    , "6543456"
    , "7555557"
    , "8555558"
    , "9555559"
    ]

rawMat2 :: [String]
rawMat2 =
    [ "5590559"
    , "5551598"
    , "5552557"
    , "6543456"
    , "7655987"
    , "8765555"
    , "9875555"
    ]

parseMat :: [String] -> Matrix Int
parseMat lns = toMatrix (map (map (read.(:[]))) lns)

fromMatrixComps :: [Comparation HikesList]
fromMatrixComps =
    [ Comparation (101, HikesList [(3, 1, 1)], fromMatrix (parseMat rawMat1) (3, 0))
    , Comparation (102, HikesList [(2, 3, 4), (4, 3, 4)], fromMatrix (parseMat rawMat1) (3, 3))
    , Comparation (103, HikesList [(6, 0, 9), (5, 1, 9)], fromMatrix (parseMat rawMat2) (6, 1))
    ]

getScore :: Matrix Int -> (Int, Int) -> Int
getScore m (x, y) = let result :: HikesList
                        result = foldl
                                    (\(HikesList hs) _ -> HikesList (concatMap (\(xh, yh, _) -> runHikesL (fromMatrix m (xh, yh))) hs))
                                    (fromMatrix m (x, y))
                                    [2..9]
                    in (length . DL.nub . map (\(xh, yh, _) -> (xh, yh))) (runHikesL result)

getHikingStarts :: Matrix Int -> [(Int, Int, Int)]
getHikingStarts (Matrix ps) = map
                                (\(x, y, _) -> (x, y, getScore (Matrix ps) (x, y)))
                                (filter (\(_, _, p) -> p == 0) ps)


getHikingStartsComps :: [Comparation [(Int, Int, Int)]]
getHikingStartsComps =
    [ Comparation (701, [(3, 0, 2)], getHikingStarts (parseMat rawMat1))
    , Comparation (702, [(3, 0, 4)], getHikingStarts (parseMat rawMat2))
    ]

testAll :: IO ()
testAll = do
    (putStrLn . unlines . map show) fromMatrixComps
    (putStrLn . unlines . map show) getHikingStartsComps

{-
answer :: IO ()
answer = do
    mass <- getFileLines "day-10.txt"
    (print . sum . map (\(x, y, sc) -> toInteger sc) . getHikingStarts . fromMass) mass
fromMass :: [String] -> Matrix Int
fromMass = parseMat . filter ((>=1).length)
-}

getRating :: Matrix Int -> (Int, Int) -> Int
getRating m (x, y) = let result :: HikesList
                         result = foldl
                                    (\(HikesList hs) _ -> HikesList (concatMap (\(xh, yh, _) -> runHikesL (fromMatrix m (xh, yh))) hs))
                                    (fromMatrix m (x, y))
                                    [2..9]
                    in length (runHikesL result)

getHikingStarts2 :: Matrix Int -> [(Int, Int, Int)]
getHikingStarts2 (Matrix ps) = map
                                (\(x, y, _) -> (x, y, getRating (Matrix ps) (x, y)))
                                (filter (\(_, _, p) -> p == 0) ps)

getHikingStarts2Comps :: [Comparation [(Int, Int, Int)]]
getHikingStarts2Comps =
    [ Comparation (1001, [(3, 0, 2)], getHikingStarts2 (parseMat rawMat1))
    , Comparation (1002, [(3, 0, 13)], getHikingStarts2 (parseMat rawMat2))
    ]

testAll2 :: IO ()
testAll2 = (putStrLn . unlines . map show) getHikingStarts2Comps

{-
answer2 :: IO ()
answer2 = do
    mass <- getFileLines "day-10.txt"
    (print . sum . map (\(x, y, sc) -> toInteger sc) . getHikingStarts2 . fromMass) mass
    -}
