{-# LANGUAGE RankNTypes #-}

-- module Day20 (testAll, answer, testAll2, answer2) where
module Day20 (testAll, testAll2) where

import qualified Data.List as DL
import qualified Data.Maybe as DM
import qualified Data.HashMap as DH
import Data.HashMap (Map)
import Data.Hashable (Hashable (hashWithSalt))

import TestLibrary (Assertable, toAssertion, toBasicAssertion, toEnumAssertion, assert, enumAssert, Label, label, runEverything, markProperty)
import Data.List ((!?))
import DataMassParsers (getFileLines)

newtype Point = Point (Int, Int)
  deriving (Eq, Show)

instance Ord Point where
  compare (Point (x, y)) (Point (x', y'))
    | x == x' = compare y y'
    | otherwise = compare x x'

instance Hashable Point where
  hashWithSalt salt (Point (x, y)) =
    hashWithSalt salt (x, y)

data Sort n = MakeSort (Sort n) n (Sort n)
            | NoSort
  deriving (Eq)

instance Show a => Show (Sort a) where
  show NoSort = ""
  show (MakeSort l x r) = 
    prependLines "  " (show l) 
      ++ "\n" ++ show x
      ++ "\n" ++ prependLines "  " (show r)
    where 
      prependLines :: String -> String -> String
      prependLines prefix =
        unlines . map (prefix++) . lines

instance Label Sort where
  label NoSort = []
  label (MakeSort l x r) =
    map (\(lab, e) -> ("L>"++lab, e)) (label l)
      ++ [("Node", x)]
      ++ map (\(lab, e) -> ("R>"++lab, e)) (label r)

pushSort :: Ord n => n -> Sort n -> Sort n
pushSort a NoSort = MakeSort NoSort a NoSort
pushSort a (MakeSort l e r) =
  if a > e
     then MakeSort l e (pushSort a r)
     else MakeSort (pushSort a l) e r

pushSortOnlyIfNotElem :: Ord n => n -> Sort n -> Sort n
pushSortOnlyIfNotElem a NoSort = MakeSort NoSort a NoSort
pushSortOnlyIfNotElem a xs
  | elemSort a xs = xs
  | otherwise = pushSort a xs

popSort :: Sort n -> Sort n
popSort NoSort = NoSort
popSort (MakeSort NoSort _ NoSort) = NoSort
popSort (MakeSort NoSort _ r) = r
popSort (MakeSort l e r) = MakeSort (popSort l) e r

elemSort :: Ord a => a -> Sort a -> Bool
elemSort _ NoSort = False
elemSort y (MakeSort l e r)
  | y == e = True
  | y < e = elemSort y l
  | otherwise = elemSort y r

leastOfSort :: Sort a -> Maybe a
leastOfSort NoSort = Nothing
leastOfSort (MakeSort NoSort x _) = Just x
leastOfSort (MakeSort x _ _) = leastOfSort x

findInSort :: Ord a => a -> Sort a -> Maybe a
findInSort _ NoSort = Nothing
findInSort y (MakeSort l e r)
  | y == e = Just e
  | y < e = findInSort y l
  | otherwise = findInSort y r

findManyInSort :: forall a. Ord a => a -> Sort a -> [a]
findManyInSort _ NoSort = []
findManyInSort y (MakeSort l e r)
  | y == e = e:(findOnlyEquals y l) ++ (findOnlyEquals y r)
  | y < e = findManyInSort y l
  | otherwise = findManyInSort y r
  where findOnlyEquals :: Ord a => a -> Sort a -> [a]
        findOnlyEquals _ NoSort = []
        findOnlyEquals x (MakeSort xL xE xR)
          | x == xE = xE:(findOnlyEquals xE xL) ++ (findOnlyEquals xE xR)
          | otherwise = []

instance Foldable Sort where
  foldr _ initialV NoSort = initialV
  foldr f initialV (MakeSort l x r) =
    let rightY = foldr f initialV r
        currentY = f x rightY
    in foldr f currentY l


data MapSlot = Start | Free | Block | Exit deriving (Eq, Show)

data Path = Path
  { currentP :: Point
  , toP :: Point
  , previousPoints :: [Point]
  , matrixBounds :: (Int, Int)
  }
  deriving (Eq)

instance Show Path where
  show ph =
    let showPointChar :: Int -> Int -> Char
        showPointChar x y
          | (Point (x, y)) `elem` (previousPoints ph) = 'O'
          | (toP ph) == Point (x, y) = '*'
          | (currentP ph) == Point (x, y) = 'O'
          | otherwise = ' '
    in concatMap
         (\y -> "\n  "
                  ++ (map
                        (\x -> showPointChar x y)
                        [0..fst (matrixBounds ph)]
                      )
          )
         [0..snd (matrixBounds ph)]
        

instance Show k => Label (Map k) where
  label = map (\(k, v) -> (show k, v)) . DH.assocs

instance Assertable x => Assertable (Maybe x) where
  toAssertion a b
    | DM.isJust a /= DM.isJust b = toAssertion a b
    | DM.isNothing a = []
    | otherwise = toAssertion (DM.fromJust a) (DM.fromJust b)

instance Assertable Point where
  toAssertion = toBasicAssertion

instance Assertable Path where
  toAssertion a b =
    (markProperty "currentP" $ toBasicAssertion (currentP a) (currentP b))
      ++ (markProperty "toP" $ toBasicAssertion (toP a) (toP b))
      ++ (markProperty "previousPoints" $ toEnumAssertion (previousPoints a) (previousPoints b))
      ++ (markProperty "matrixBounds" $ toBasicAssertion (matrixBounds a) (matrixBounds b))


pointsNext :: [[MapSlot]] -> (Int, Int) -> [(Int, Int)]
pointsNext m (x, y) =
  let mWidth :: Int
      mWidth = (length . (!!0)) m
      mHeight :: Int
      mHeight = length m
  in [(x', y') | (x', y') <- [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
        , 0 <= x' && x' <= (mWidth - 1) 
          && 0 <= y' && y' <= (mHeight - 1)
          && ((m !! y') !! x') `elem` [Free, Exit] 
     ]

pointsNext_ :: [[MapSlot]] -> Point -> [Point]
pointsNext_ matx = map Point . pointsNext matx . (\(Point p) -> p)

findPath :: [[MapSlot]] -> Path -> Maybe Path
findPath matx ph =
  let pointsNextToPh :: [Point]
      pointsNextToPh = filter (`notElem` (previousPoints ph))
                          $ pointsNext_ matx (currentP ph)
  in (!? 0)
      $ map
          (\p -> Path
            { currentP = p
            , toP = toP ph
            , previousPoints = previousPoints ph ++ [currentP ph]
            , matrixBounds = matrixBounds ph
            })
          pointsNextToPh

findMainPath :: [[MapSlot]] -> Path -> Maybe Path
findMainPath matx ph =
  do 
    pathFound <- findPath matx ph
    if currentP pathFound == toP pathFound
        then return pathFound
        else findMainPath matx pathFound

findIndexInMatrix :: (a -> Bool) -> [[a]] -> Maybe Point
findIndexInMatrix f xs = do
  yIndex <- DL.findIndex (any f) xs
  xIndex <- DL.findIndex f (xs !! yIndex)
  return $ Point (xIndex, yIndex)

getStart :: [[MapSlot]] -> Path
getStart m = 
  let mWidth :: Int
      mWidth = (length . (!!0)) m
      mHeight :: Int
      mHeight = length m
      startP :: Point
      startP = DM.fromJust $ findIndexInMatrix (== Start) m
      endP :: Point
      endP = DM.fromJust $ findIndexInMatrix (== Exit) m
  in Path
      { currentP = startP
      , toP = endP
      , previousPoints = []
      , matrixBounds = (mWidth , mHeight)
      }

matTest1 :: [[MapSlot]]
matTest1 =
  [ [Free,  Free,  Free,  Block, Block]
  , [Free,  Block, Exit,  Block, Block]
  , [Free,  Block, Block, Block, Block]
  , [Free,  Free,  Free,  Block, Start]
  , [Block, Block, Free,  Free,  Free]
  ]

testMainPath :: [Maybe String]
testMainPath =
  [ assert (Point (4, 3)) (currentP $ getStart matTest1) "101"
  , assert (Point (2, 1)) (toP $ getStart matTest1) "102"
  , assert
      (Just
        $ Path
            { currentP = Point (2, 1)
            , toP = Point (2, 1)
            , previousPoints = map Point [(4, 3), (4, 4), (3, 4), (2, 4), (2, 3), (1, 3), (0, 3), (0, 2), (0, 1), (0, 0), (1, 0), (2, 0)]
            , matrixBounds = (5, 5)
            })
      (findMainPath matTest1 (getStart matTest1))
      "103"
  ]

findCheats :: (Int, Int) -> Int -> Path -> Map Point Int -> [Point]
findCheats (x, y) required ph xs =
  [Point (x', y') | (x', y') <- [(x + 2, y), (x - 2, y), (x, y + 2), (x, y - 2)]
      , 0 <= x' && x' <= (fst (matrixBounds ph) - 2)
        && 0 <= y' && y' <= (snd (matrixBounds ph) - 2)

        && DH.member (Point (x, y)) xs && DH.member (Point (x', y')) xs
        && pointIndex (x', y') - pointIndex (x, y) >= (required + 2)
  ]
  where pointIndex :: (Int, Int) -> Int
        pointIndex = (\de -> DH.findWithDefault 0 de xs) . Point

cheatWaysCount :: Maybe Int -> Path -> Int
cheatWaysCount minGainRequired finishedGame =
  let required = DM.fromMaybe 1 minGainRequired
      doneMap :: Map Point Int
      doneMap =
        DH.fromList
          $ zipWith (\d e -> (e, d)) [0..] (previousPoints finishedGame ++ [toP finishedGame])
  in length
      $ concatMap (\(Point p) -> findCheats p required finishedGame doneMap)
      $ previousPoints finishedGame

instance Assertable Int where
  toAssertion = toBasicAssertion

testCheat :: [Maybe String]
testCheat =
  [ assert 3 (cheatWaysCount Nothing solvedMat1) "201"
  , assert 3 (cheatWaysCount (Just 2) solvedMat1) "202"
  , assert 1 (cheatWaysCount (Just 4) solvedMat1) "203"
  , assert 1 (cheatWaysCount (Just 6) solvedMat1) "204"
  , assert 0 (cheatWaysCount (Just 7) solvedMat1) "205"
  ]
  where solvedMat1 :: Path
        solvedMat1 = DM.fromJust $ findMainPath matTest1 (getStart matTest1) 

readWholeMatrix :: [String] -> [[MapSlot]]
readWholeMatrix =
  map
    (map (\c -> 
        case c of
              'S' -> Start
              'E' -> Exit
              '#' -> Block
              '.' -> Free
              _   -> Block))

testAll :: IO ()
testAll = runEverything
  $ testMainPath
    ++ testCheat

{-
answer :: IO ()
answer = do
  mass <- getFileLines "day-20.txt"
  let matrix = readWholeMatrix mass
  print (
    do
      mainPath <- findMainPath matrix (getStart matrix)
      return $ cheatWaysCount (Just 100) mainPath
    )
  -}

calcDistance :: Point -> Point -> Int
calcDistance (Point (t, u)) (Point (t', u')) = abs (t - t') + abs (u - u')

bigCheatWaysCount :: Maybe Int -> Path -> Int
bigCheatWaysCount minGainRequired finishedGame =
  let required = DM.fromMaybe 1 minGainRequired
      allTrack :: [Point]
      allTrack = previousPoints finishedGame ++ [toP finishedGame]
      doneMap :: Map Point Int
      doneMap =
        DH.fromList $ zipWith (\d e -> (e, d)) [0..] allTrack
  in sum
        $ map
            (\e -> length
                    $ filter
                        (\c ->
                          (pointIndex c doneMap - pointIndex e doneMap) >= (required + calcDistance c e))
                    $ filter
                        (\c ->
                          (pointIndex c doneMap - pointIndex e doneMap >= required)
                            && calcDistance c e <= 20)
                        allTrack
            )
            allTrack
  where pointIndex :: Point -> Map Point Int -> Int
        pointIndex = DH.findWithDefault 0

matTest2 :: [[MapSlot]]
matTest2 =
  [ [Block, Block, Block, Block, Block, Block, Block, Block, Block, Block, Block, Block, Block, Block, Block]
  , [Block, Free , Free , Free , Block, Free , Free , Free , Block, Free , Free , Free , Free , Free , Block]
  , [Block, Free , Block, Free , Block, Free , Block, Free , Block, Free , Block, Block, Block, Free , Block]
  , [Block, Start, Block, Free , Free , Free , Block, Free , Block, Free , Block, Free , Free , Free , Block]
  , [Block, Block, Block, Block, Block, Block, Block, Free , Block, Free , Block, Free , Block, Block, Block]
  , [Block, Block, Block, Block, Block, Block, Block, Free , Block, Free , Block, Free , Free , Free , Block]
  , [Block, Block, Block, Block, Block, Block, Block, Free , Block, Free , Block, Block, Block, Free , Block]
  , [Block, Block, Block, Free , Free , Exit , Block, Free , Free , Free , Block, Free , Free , Free , Block]
  , [Block, Block, Block, Free , Block, Block, Block, Block, Block, Block, Block, Free , Block, Block, Block]
  , [Block, Free , Free , Free , Block, Block, Block, Free , Free , Free , Block, Free , Free , Free , Block]
  , [Block, Free , Block, Block, Block, Block, Block, Free , Block, Free , Block, Block, Block, Free , Block]
  , [Block, Free , Block, Free , Free , Free , Block, Free , Block, Free , Block, Free , Free , Free , Block]
  , [Block, Free , Block, Free , Block, Free , Block, Free , Block, Free , Block, Free , Block, Block, Block]
  , [Block, Free , Free , Free , Block, Free , Free , Free , Block, Free , Free , Free , Block, Block, Block]
  , [Block, Block, Block, Block, Block, Block, Block, Block, Block, Block, Block, Block, Block, Block, Block]
  ]

testBigCheat :: [Maybe String]
testBigCheat =
  [ assert 4 (calcDistance (Point (4, 3)) (Point (2, 1))) "1001"
  , assert 1 (bigCheatWaysCount (Just 8) solvedMat1) "1002"
  , assert 6 (bigCheatWaysCount (Just 6) solvedMat1) "1003"
  , assert 3 (bigCheatWaysCount (Just 76) solvedMat2) "1101"
  , assert 7 (bigCheatWaysCount (Just 74) solvedMat2) "1102"
  , assert 29 (bigCheatWaysCount (Just 72) solvedMat2) "1103"
  , assert 41 (bigCheatWaysCount (Just 70) solvedMat2) "1104"
  , assert 55 (bigCheatWaysCount (Just 68) solvedMat2) "1105"
  , assert 67 (bigCheatWaysCount (Just 66) solvedMat2) "1106"
  , assert 86 (bigCheatWaysCount (Just 64) solvedMat2) "1107"
  , assert 106 (bigCheatWaysCount (Just 62) solvedMat2) "1108"
  , assert 129 (bigCheatWaysCount (Just 60) solvedMat2) "1109"
  , assert 154 (bigCheatWaysCount (Just 58) solvedMat2) "1110"
  , assert 193 (bigCheatWaysCount (Just 56) solvedMat2) "1111"
  , assert 222 (bigCheatWaysCount (Just 54) solvedMat2) "1112"
  , assert 253 (bigCheatWaysCount (Just 52) solvedMat2) "1113"
  , assert 285 (bigCheatWaysCount (Just 50) solvedMat2) "1114"
  ]
  where solvedMat1 :: Path
        solvedMat1 = DM.fromJust $ findMainPath matTest1 (getStart matTest1) 
        solvedMat2 :: Path
        solvedMat2 = DM.fromJust $ findMainPath matTest2 (getStart matTest2)

testAll2 :: IO ()
testAll2 = runEverything
  testBigCheat

{-
answer2 :: IO ()
answer2 = do
  mass <- getFileLines "day-20.txt"
  let matrix = readWholeMatrix mass
  print (
    do
      mainPath <- findMainPath matrix (getStart matrix)
      return $ bigCheatWaysCount (Just 100) mainPath
    )
  -}
