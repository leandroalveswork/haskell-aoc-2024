module Day13 (testAll, testAll2) where
--module Day13 (testAll, answer, testAll2, answer2) where

import qualified Data.List as DL
import qualified Data.List.Split as DLS
import qualified Data.Maybe as DM

import TestLib (Comparation(Comparation))
import DataMassParsers (getFileLines)

newtype Vec = Vec (Int, Int)

{-

A: (vx, vy)
B: (vx', vy')
S: (vxs, vys)

m * (vx, vy) + n * (vx', vy') = (vxs, vys)

-- unique if (vx / vy) /=  (vx', vy')
-- only m and n are variables
m * vx + n * vx' = vxs
m * vy + n * vy' = vys

m * (vx - (vy * vx' / vy')) = vxs - (vys * vx' / vy')
m = (vxs - (vys * vx' / vy')) / (vx - (vy * vx' / vy'))

n * (vx' - (vy' * vx / vy)) = vxs - (vys * vx / vy)
n = (vxs - (vys * vx / vy)) / (vx' - (vy' * vx / vy))

-- if (vx / vy) ==  (vx', vy'), check if it is possible for both to divide the whole path
-- if only one is possible, pick the unique
-- if both are possible, compare if A vet / B vet is > 3. if true, then use A, else, use B

-}

getMultipliers :: Vec -> Vec -> Vec -> Maybe (Int, Int)
getMultipliers (Vec (vx, vy)) (Vec (vx', vy')) (Vec (vxs, vys))
    | ratDiv vx vy == ratDiv vx' vy'= case (vxs `mod` vx, vxs `mod` vx') of
                                           (0, 0) -> if vx > vx' * 3 then Just (vxs `div` vx, 0)
                                                                     else Just (0, vxs `div` vx')
                                           (0, _) -> Just (vxs `div` vx, 0)
                                           (_, 0) -> Just (0, vxs `div` vx')
                                           (_, _) -> Nothing
    | otherwise = let m :: Rational
                      m = (toRational vxs - toRational vys * ratDiv vx' vy') / (toRational vx - toRational vy * ratDiv vx' vy')
                      n :: Rational
                      n = (toRational vxs - toRational vys * ratDiv vx vy) / (toRational vx' - toRational vy' * ratDiv vx vy)
                  in if toRational (floor m) == m && toRational (floor n) == n then Just (floor m, floor n) else Nothing
    where ratDiv :: Int -> Int -> Rational
          ratDiv a b = toRational a / toRational b

getMultipliersComps :: [Comparation (Maybe (Int, Int))]
getMultipliersComps =
    [ Comparation (101, Just (80, 40), getMultipliers (Vec (94, 34)) (Vec (22, 67)) (Vec (8400, 5400)))
    , Comparation (102, Nothing, getMultipliers (Vec (26, 66)) (Vec (67, 21)) (Vec (12748, 12176)))
    , Comparation (103, Just (38, 86), getMultipliers (Vec (17, 86)) (Vec (84, 37)) (Vec (7870, 6450)))
    , Comparation (104, Nothing, getMultipliers (Vec (69, 23)) (Vec (27, 71)) (Vec (18641, 10279)))
    ]

testAll :: IO ()
testAll = (putStrLn . unlines . map show) getMultipliersComps

{-
answer :: IO ()
answer = do
    mass <- getFileLines "day-13.txt"
    print $ sum $ getCosts $ map (\(v, v', vs) -> getMultipliers v v' vs) $ fromMass mass
    where getCosts :: [Maybe (Int, Int)] -> [Integer]
          getCosts = map (maybe 0 (\(m, n) -> toInteger (m * 3 + n)))

fromMass :: [String] -> [(Vec, Vec, Vec)]
fromMass = groupTriples
            . map parseLine
            . filter ((>=1).length)
    where parseLine :: String -> Vec
          parseLine s = let realS :: String
                            realS = (DLS.splitOn ":" s) !! 1
                        in Vec ((read . drop 3 . head . DLS.splitOn ",") realS, (read . drop 3 . (!!1) . DLS.splitOn ",") realS)
          groupTriples :: [Vec] -> [(Vec, Vec, Vec)]
          groupTriples (m:n:o:ts) = (m, n, o):groupTriples ts
          groupTriples _          = []
          -}

newtype VecBig = VecBig (Integer, Integer)

getMultipliersBig :: VecBig -> VecBig -> VecBig -> Maybe (Integer, Integer)
getMultipliersBig (VecBig (vx, vy)) (VecBig (vx', vy')) (VecBig (vxs, vys))
    | ratDiv vx vy == ratDiv vx' vy'= case (vxs `mod` vx, vxs `mod` vx') of
                                           (0, 0) -> if vx > vx' * 3 then Just (vxs `div` vx, 0)
                                                                     else Just (0, vxs `div` vx')
                                           (0, _) -> Just (vxs `div` vx, 0)
                                           (_, 0) -> Just (0, vxs `div` vx')
                                           (_, _) -> Nothing
    | otherwise = let m :: Rational
                      m = (toRational vxs - toRational vys * ratDiv vx' vy') / (toRational vx - toRational vy * ratDiv vx' vy')
                      n :: Rational
                      n = (toRational vxs - toRational vys * ratDiv vx vy) / (toRational vx' - toRational vy' * ratDiv vx vy)
                  in if toRational (floor m) == m && toRational (floor n) == n then Just (floor m, floor n) else Nothing
    where ratDiv :: Integer -> Integer -> Rational
          ratDiv a b = toRational a / toRational b

getAppliedMults :: VecBig -> VecBig -> VecBig -> Maybe (Integer, Integer)
getAppliedMults v v' (VecBig (vxs, vys)) = getMultipliersBig v v' (VecBig (vxs + 10000000000000, vys + 10000000000000))

getAppliedMultsComps :: [Comparation Bool]
getAppliedMultsComps =
    [ Comparation (1001, False, DM.isJust (getAppliedMults (VecBig (94, 34)) (VecBig (22, 67)) (VecBig (8400, 5400))))
    , Comparation (1002, True, DM.isJust (getAppliedMults (VecBig (26, 66)) (VecBig (67, 21)) (VecBig (12748, 12176))))
    , Comparation (1003, False, DM.isJust (getAppliedMults (VecBig (17, 86)) (VecBig (84, 37)) (VecBig (7870, 6450))))
    , Comparation (1004, True, DM.isJust (getAppliedMults (VecBig (69, 23)) (VecBig (27, 71)) (VecBig (18641, 10279))))
    ]

testAll2 :: IO ()
testAll2 = (putStrLn . unlines . map show) getAppliedMultsComps

{-
fromVec :: Vec -> VecBig
fromVec (Vec (vx, vy)) = VecBig (toInteger vx, toInteger vy)

answer2 :: IO ()
answer2 = do
    mass <- getFileLines "day-13.txt"
    print $ sum $ getCosts $ map (\(v, v', vs) -> getAppliedMults (fromVec v) (fromVec v') (fromVec vs)) $ fromMass mass
    where getCosts :: [Maybe (Integer, Integer)] -> [Integer]
          getCosts = map (maybe 0 (\(m, n) -> m * 3 + n))
          -}
