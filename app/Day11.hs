module Day11 (testAll, testAll2) where
--module Day11 (testAll, answer, testAll2, answer2) where

import qualified Data.List as DL

import Debugging (DWriter(DWriter), debug, getWritten, tell)
import TestLib (Comparation(Comparation))
import DataMassParsers (getFileLines)

blinkNumber :: Integer -> [Integer]
blinkNumber x
    | x == 0                 = [1]
    | even (length (show x)) = let (a, b) = splitAt (length (show x) `div` 2) (show x)
                               in [read a, read b]
    | otherwise              = [x * 2024]

blinkTimes :: Int -> [Integer] -> [Integer]
blinkTimes n xs = foldl (\zs _ -> concatMap blinkNumber zs) xs [1..n]

blinkTimesComps :: [Comparation [Integer]]
blinkTimesComps =
    [ Comparation (101, [253000, 1, 7], blinkTimes 1 [125, 17])
    , Comparation (102, [253, 0, 2024, 14168], blinkTimes 2 [125, 17])
    , Comparation (103, [1036288, 7, 2, 20, 24, 4048, 1, 4048, 8096, 28, 67, 60, 32], blinkTimes 5 [125, 17])
    ]

testAll :: IO ()
testAll = (putStrLn . unlines . map show) blinkTimesComps

{-
mass :: [Integer]
mass = [890, 0, 1, 935698, 68001, 3441397, 7221, 27]

answer :: IO ()
answer = (print . length . blinkTimes 25) mass
-}

newtype Repeated a = Repeated (a, Integer)
    deriving Eq

instance Functor Repeated where
    fmap f (Repeated (x, qt)) = Repeated (f x, qt)

instance Show a => Show (Repeated a) where
    show (Repeated (x, qt)) = show x ++ "x" ++ show qt

asRepeated :: a -> Repeated a
asRepeated x = Repeated (x, 1)

joinRepeats :: Eq a => [Repeated a] -> [Repeated a]
joinRepeats = foldl
                (\zs (Repeated (x, qt)) -> (map
                                            (\(Repeated (z, zqt)) -> if z == x then Repeated (z, zqt + qt) else Repeated (z, zqt))
                                            zs)
                                            ++ if any (\(Repeated (z, zqt)) -> z == x) zs then [] else [(Repeated (x, qt))]
                )
                []

joinRepeatsComps :: [Comparation [Repeated Char]]
joinRepeatsComps =
    [ Comparation (1001, [Repeated ('C', 4)], joinRepeats [Repeated ('C', 2), Repeated ('C', 1), Repeated ('C', 1)])
    , Comparation (1002, [Repeated ('A', 7), Repeated ('C', 30)], joinRepeats [Repeated ('A', 4), Repeated ('C', 1), Repeated ('C', 29), Repeated ('A', 3)])
    ]

blinkTimesOnlyForLength :: Int -> [Repeated Integer] -> [Repeated Integer]
blinkTimesOnlyForLength n xs = foldl (\zs _ -> (joinRepeats . concatMap blinkAndFlatten) zs) xs [1..n]
    where blinkAndFlatten :: Repeated Integer -> [Repeated Integer]
          blinkAndFlatten (Repeated (x, qt)) = map (\n -> Repeated (n, qt)) (blinkNumber x)

repeatedLength :: [Repeated a] -> Integer
repeatedLength = sum . map (\(Repeated (_, qt)) -> qt)

blinkTimesOnlyForLengthComps :: [Comparation Integer]
blinkTimesOnlyForLengthComps =
    [ Comparation (1301, 55312, (repeatedLength . blinkTimesOnlyForLength 25 . map asRepeated) [125, 17])
    , Comparation (1302, 194782, (repeatedLength . blinkTimesOnlyForLength 25 . map asRepeated) mass)
    ]

testAll2 :: IO ()
testAll2 = do
    (putStrLn . unlines . map show) joinRepeatsComps
    (putStrLn . unlines . map show) blinkTimesOnlyForLengthComps

    {-
answer2 :: IO ()
answer2 = (print . repeatedLength . blinkTimesOnlyForLength 75 . map asRepeated) mass
-}
