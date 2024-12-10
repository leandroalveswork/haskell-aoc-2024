module Day05 (testAll, testAll2) where
--module Day05 (testAll, answer, testAll2, answer2) where

import qualified Data.List.Split as DLS
import qualified Data.Maybe as DM
import qualified Data.List as DL
import qualified Text.Read as TR

import TestLib (Comparation(Comparation))
import DataMassParsers (getFileLines)

itemViolatesRule :: [Int] -> (Int, Int) -> (Int, Int) -> Bool
itemViolatesRule xs (pos, x) (minx, maxx)
    | minx == x = elem maxx (take pos xs)
    | maxx == x = elem minx (drop (pos + 1) xs)
    | otherwise = False

orderViolates :: [Int] -> [(Int, Int)] -> Bool
orderViolates xs rules = let indexedXs = zip [0..] xs
                         in or (itemViolatesRule xs <$> indexedXs <*> rules)

rules1 :: [(Int, Int)]
rules1 =
    [(47,53), (97,13), (97,61), (97,47), (75,29), (61,13), (75,53), (29,13), (97,29), (53,29), (61,53),
        (97,53), (61,29), (47,13), (75,47), (97,75), (47,61), (75,61), (47,29), (75,13), (53,13)
    ]

violatesComps :: [Comparation Bool]
violatesComps =
    [ Comparation (1, False, orderViolates [75,47,61,53,29] rules1)
    , Comparation (2, False, orderViolates [97,61,53,29,13] rules1)
    , Comparation (3, False, orderViolates [75,29,13] rules1)
    , Comparation (4, True, orderViolates [75,97,47,61,53] rules1)
    , Comparation (5, True, orderViolates [61,13,29] rules1)
    , Comparation (6, True, orderViolates [97,13,75,29,47] rules1)
    ]

middle :: [Int] -> Maybe Int
middle [] = Nothing
middle xs = Just (xs !! ((length xs) `div` 2))

middleComps :: [Comparation (Maybe Int)]
middleComps =
    [ Comparation (11, Nothing, middle [])
    , Comparation (12, Just 61, middle [75,47,61,53,29])
    , Comparation (13, Just 53, middle [97,61,53,29,13])
    , Comparation (14, Just 29, middle [75,29,13])
    , Comparation (15, Just 7, middle [7])
    , Comparation (15, Just 80, middle [8, 80])
    ]

testAll :: IO ()
testAll = do
    (putStrLn . unlines . map show) violatesComps
    (putStrLn . unlines . map show) middleComps

answer :: IO ()
answer = do
    mass <- getFileLines "day-05.txt"
    print $ stupidFunction $ fromMass mass
    where stupidFunction :: ([(Int, Int)], [[Int]]) -> Int
          stupidFunction (rs, orders) = let correctOrders = filter (\l -> not (orderViolates l rs)) orders
                                            middleNumbers = map (DM.fromMaybe 0 . middle) correctOrders
                                        in sum middleNumbers

fromMass :: [String] -> ([(Int, Int)], [[Int]])
fromMass l = let breaksAt =  DL.findIndices ((==0).length) l
             in case breaksAt of
                     []    -> ([], []) -- Impossible, get out with this invalid result
                     (p:_) -> (map readRule (take p l), map readOrder (drop (p + 1) l))
    where readRule :: String -> (Int, Int)
          readRule cs = let (a:b:_) = DLS.splitOn "|" cs in (TR.read a, TR.read b)
          readOrder :: String -> [Int]
          readOrder = (map TR.read) . DLS.splitOn ","

orderViolations :: [Int] -> [(Int, Int)] -> [(Int, Int)]
orderViolations xs rules = let indexedXs = zip [0..] xs
                               violations = (violatesAndReturnRule xs <$> indexedXs <*> rules)
                           in  map snd (filter fst violations)
    where violatesAndReturnRule :: [Int] -> (Int, Int) -> (Int, Int) -> (Bool, (Int, Int))
          violatesAndReturnRule xs' item rule = (itemViolatesRule xs' item rule, rule)

data SolvedLine = SolvedLine
    { sItem :: Int
    , sRules :: [(Int, Int)]
    }

instance Eq SolvedLine where
    a == b = sItem a == sItem b && sRules a == sRules b

solveAndRemember :: SolvedLine -> y -> [SolvedLine] -> Maybe (y, [SolvedLine])
solveAndRemember sx y solvs = if length (DL.findIndices (==sx) solvs) > 3
                                then Nothing
                                else Just (y, solvs ++ [sx])

filterByMin :: Int -> [(Int, Int)] -> [(Int, Int)]
filterByMin x = filter ((==x).snd)
filterByMax :: Int -> [(Int, Int)] -> [(Int, Int)]
filterByMax x = filter ((==x).fst)

moveNumberTo :: Int -> Int -> [Int] -> [Int]
moveNumberTo posit n xs = let filtered = filter (/=n) xs
                              afterOffset = fmap ((\b -> if b then 1 else 0).(<posit)) (DL.findIndex (==n) xs)
                          in case afterOffset of
                                  Nothing  -> xs
                                  Just af' -> take (posit - af') filtered ++ [n] ++ drop (posit - af') filtered

fixForRules :: [Int] -> Int -> [(Int, Int)] -> [Int]
fixForRules xs wrong rs
    | length (filterByMin wrong rs) > 0 && length (filterByMax wrong rs) == 0  =
        case DL.findIndex (`elem` map fst (filterByMin wrong rs)) xs of
             Nothing       -> xs
             Just leastPos -> moveNumberTo (leastPos + 1) wrong xs
    | length (filterByMin wrong rs) == 0 && length (filterByMax wrong rs) > 0 =
        case fmap ((length xs - 1) -) (DL.findIndex (`elem` map snd (filterByMax wrong rs)) (reverse xs)) of
             Nothing      -> xs
             Just mostPos -> moveNumberTo mostPos wrong xs
      -- have both byMin and byMax
    | otherwise = let maxesToMove = filter (`elem` map snd (filterByMax wrong rs)) xs
                  in case DL.findIndex (`elem` map fst (filterByMin wrong rs)) xs of
                          Nothing       -> xs
                          Just leastPos -> foldl
                                                (\curL (shif, maxN) -> moveNumberTo (leastPos + 1 + shif) maxN curL)
                                                (moveNumberTo (leastPos + 1) wrong xs)
                                                (zip [1..] maxesToMove)


-- The preorder may be cycled: if the fixing can't be done due to a cycle, return Nothing
fixOrder :: [Int] -> [(Int, Int)] -> [SolvedLine] -> Maybe [Int]
fixOrder []  allRules solveds = Just []
fixOrder [x] allRules solveds = Just [x]
fixOrder xs  allRules solveds = let xsRules    = orderViolations xs allRules
                                    wrongXs    = DL.nub (concatMap (\(a, b) -> [a, b]) xsRules)
                                    firstToFix = foldl
                                                    (\acc ni -> Just ni)
                                                    Nothing
                                                    (filter ((>0).fst) (map rankPriorities wrongXs))
                                        where rankPriorities :: Int -> (Int, Int)
                                              rankPriorities x = let minesLen = length (filterByMin x xsRules)
                                                                     maxesLen = length (filterByMax x xsRules)
                                                                 in if minesLen > 0 && maxesLen == 0
                                                                       then (2000000 + minesLen, x)
                                                                       else if minesLen == 0 && maxesLen > 0
                                                                       then (1000000 + maxesLen, x)
                                                                       else if minesLen > 0 && maxesLen > 0
                                                                       then (minesLen * 1000 + maxesLen, x)
                                                                       else (0, x)
                                    in case firstToFix of
                                            Nothing       -> Just xs
                                            Just (_,next) -> let result    = fixForRules xs next xsRules
                                                                 newSolved = SolvedLine { sItem = next, sRules = xsRules }
                                                             in do
                                                                 (mResult, newSolveds) <- solveAndRemember newSolved result solveds
                                                                 fixOrder mResult allRules newSolveds
moveNumberComps :: [Comparation [Int]]
moveNumberComps =
    [ Comparation (136, [97,75,47,61,53], moveNumberTo 0 97 [75,97,47,61,53])
    , Comparation (137, [75,47,61,97,53], moveNumberTo 4 97 [75,97,47,61,53])
    ]

rulesComps :: [Comparation [Int]]
rulesComps =
    [ Comparation (131, [97,75,47,61,53], fixForRules [75,97,47,61,53] 97 [(97,75)])
    , Comparation (132, [97,75,47,61,53], fixForRules [75,97,47,61,53] 75 [(97,75)])
    ]

fixComps :: [Comparation (Maybe [Int])]
fixComps =
    [ Comparation (101, Just [97,75,47,61,53], fixOrder [75,97,47,61,53] rules1 [])
    , Comparation (102, Just [61,29,13], fixOrder [61,13,29] rules1 [])
    , Comparation (103, Just [97,75,47,29,13], fixOrder [97,13,75,29,47] rules1 [])
    ]

testAll2 :: IO ()
testAll2 = do
    (putStrLn . unlines . map show) fixComps
    (putStrLn . unlines . map show) rulesComps
    (putStrLn . unlines . map show) moveNumberComps

answer2 :: IO ()
answer2 = do
    mass <- getFileLines "day-05.txt"
    print $ stupidFunction $ fromMass mass
    where stupidFunction :: ([(Int, Int)], [[Int]]) -> (Int, Int)
          stupidFunction (rs, orders) = let incorrectOrders = (filter (\l -> orderViolates l rs) orders)
                                            fixedOs = map (\a -> fixOrder a rs []) incorrectOrders
                                            justFixeds = map (\(Just ax) -> ax) (filter DM.isJust fixedOs)
                                            middleNumbers = map (DM.fromMaybe 0 . middle) justFixeds
                                        in (sum middleNumbers, length (filter DM.isNothing fixedOs))
