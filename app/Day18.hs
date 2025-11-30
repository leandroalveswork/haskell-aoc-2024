{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

-- module Day18 (testAll, answer, answer2) where
module Day18 (testAll) where

import qualified Data.HashMap as DH
import qualified Data.List as DL
import qualified Data.List.Split as DLS
import qualified Text.Read as TR
import Data.Hashable (Hashable (hashWithSalt))
import Data.HashMap (Map)

import TestLibrary (Assertable, toAssertion, toBasicAssertion, toEnumAssertion, assert, enumAssert, Label, label, runEverything, markProperty)
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
  , previousPoints :: Map Point ()
  , totalScore :: Int
  , matrixBounds :: (Int, Int)
  }
  deriving (Eq)

instance Show Path where
  show ph =
    let showPointChar :: Int -> Int -> Char
        showPointChar x y
          | DH.member (Point (x, y)) (previousPoints ph) = 'O'
          | (toP ph) == Point (x, y) = '*'
          | (currentP ph) == Point (x, y) = 'O'
          | otherwise = ' '
    in "{ Score = " ++ show (totalScore ph)
          ++ concatMap
               (\y -> "\n  "
                        ++ (map
                              (\x -> showPointChar x y)
                              [0..fst (matrixBounds ph)]
                            )
                )
               [0..snd (matrixBounds ph)]
          ++ "}"

instance Show k => Label (Map k) where
  label = map (\(k, v) -> (show k, v)) . DH.assocs

instance Assertable () where
  toAssertion _ _ = []

instance Assertable Path where
  toAssertion a b =
    (markProperty "currentP" $ toBasicAssertion (currentP a) (currentP b))
      ++ (markProperty "toP" $ toBasicAssertion (toP a) (toP b))
      ++ (markProperty "previousPoints" $ toEnumAssertion (previousPoints a) (previousPoints b))
      ++ (markProperty "totalScore" $ toBasicAssertion (totalScore a) (totalScore b))
      ++ (markProperty "matrixBounds" $ toBasicAssertion (matrixBounds a) (matrixBounds b))

newtype ScoredPath = ScoredPath Path

instance Eq ScoredPath where
  (ScoredPath a) == (ScoredPath b) = (totalScore a) == (totalScore b)
instance Ord ScoredPath where
  compare (ScoredPath a) (ScoredPath b) = compare (totalScore a) (totalScore b)

instance Show ScoredPath where
  show (ScoredPath scored) = "[scored]-> " ++ show scored


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

findPaths :: [[MapSlot]] -> Path -> [Path]
findPaths matx ph =
  let pointsNextToPh :: [Point]
      pointsNextToPh = filter (\x -> (DH.notMember x (previousPoints ph)))
                          $ pointsNext_ matx (currentP ph)
  in map
        (\p -> Path
          { currentP = p
          , toP = toP ph
          , previousPoints = DH.insert p () (previousPoints ph)
          , totalScore = totalScore ph + 1
          , matrixBounds = matrixBounds ph
          })
        pointsNextToPh

findLeastScorePath :: [[MapSlot]] -> Sort ScoredPath -> Map Point Path -> Maybe Path
findLeastScorePath matx phs pointedPhs =
  do
    wrappedLeastPh <- leastOfSort phs
    let remainingPhs :: Sort ScoredPath
        remainingPhs = popSort phs
        (ScoredPath leastPh) = wrappedLeastPh
        pathsFound :: [Path]
        pathsFound = findPaths matx leastPh
        pathCostsMore :: Path -> Map Point Path -> Bool
        pathCostsMore ph' kPhs = 
          case DH.lookup (currentP ph') kPhs of
                Nothing -> False
                Just kPh ->
                  case compare (totalScore ph') (totalScore kPh) of
                          LT -> False
                          GT -> True
                          EQ -> ph' /= kPh
        totalPointeds :: Map Point Path
        totalPointeds = foldl (\acc x -> if DH.member (currentP x) acc then acc else DH.insert (currentP x) x acc) pointedPhs pathsFound
      in case DL.find (\ph' -> toP ph' == currentP ph') pathsFound of
              Just phExit -> Just phExit
              Nothing -> 
                findLeastScorePath 
                  matx 
                  (foldl (\acc x -> if pathCostsMore x totalPointeds then acc else pushSort (ScoredPath x) acc) remainingPhs pathsFound)
                  totalPointeds

matTest1 :: [[MapSlot]]
matTest1 =
  [ [Free, Block, Free,  Free,  Free]
  , [Free, Free,  Free,  Block, Free]
  , [Free, Block, Block, Block, Free]
  , [Free, Block, Block, Free,  Free]
  , [Free, Free,  Free,  Free,  Exit]
  ]

instance Assertable (Int, Int) where
  toAssertion = toBasicAssertion

testPointsNext :: IO ()
testPointsNext = runEverything
  [ enumAssert [(1, 4), (0, 3)] (pointsNext matTest1 (0, 4)) "101"
  , enumAssert [(2, 0), (0, 0), (1, 1)] (pointsNext matTest1 (1, 0)) "102"
  , enumAssert [(4, 3), (3, 4)] (pointsNext matTest1 (3, 3)) "103"
  , enumAssert [(4, 2), (4, 0)] (pointsNext matTest1 (4, 1)) "104"
  ]

getStart :: [[MapSlot]] -> Path
getStart m = 
  let mWidth :: Int
      mWidth = (length . (!!0)) m
      mHeight :: Int
      mHeight = length m
  in Path
      { currentP = Point (0, 0)
      , toP = Point (mWidth - 1, mHeight - 1)
      , previousPoints = DH.empty
      , totalScore = 0
      , matrixBounds = (mWidth , mHeight)
      }

matTest2 :: [[MapSlot]]
matTest2 =
  [ [Free, Free, Free, Free,  Free,  Free,  Free, Free, Free]
  , [Free, Free, Free, Free,  Free,  Free,  Free, Free, Free]
  , [Free, Free, Free, Free,  Free,  Free,  Free, Free, Free]
  , [Free, Free, Free, Block, Block, Block, Free, Free, Free]
  , [Free, Free, Free, Block, Free,  Free,  Free, Free, Free]
  , [Free, Free, Free, Block, Free,  Free,  Free, Free, Free]
  , [Free, Free, Free, Free,  Free,  Free,  Free, Free, Free]
  , [Free, Free, Free, Free,  Free,  Free,  Free, Free, Free]
  , [Free, Free, Free, Free,  Free,  Free,  Free, Free, Exit]
  ]

matTest3 :: [[MapSlot]]
matTest3 =
  [ [Free,  Block, Free,  Free,  Free]
  , [Free,  Free,  Free,  Block, Free]
  , [Block, Block, Block, Free,  Free]
  , [Free,  Block, Block, Free,  Block]
  , [Free,  Free,  Free,  Free,  Exit]
  ]

instance Assertable (Maybe Int) where
  toAssertion = toBasicAssertion
matrixSortArg :: [[MapSlot]] -> Sort ScoredPath
matrixSortArg m =
  let sortE :: ScoredPath
      sortE = ScoredPath $ getStart m
  in MakeSort NoSort sortE NoSort

testLeastScore :: IO ()
testLeastScore = runEverything
  [ assert (Just 8) (fmap totalScore $ findLeastScorePath matTest1 (matrixSortArg matTest1) DH.empty) "201"
  , assert (Just 16) (fmap totalScore $ findLeastScorePath matTest2 (matrixSortArg matTest2) DH.empty) "202"
  , assert (Just 12) (fmap totalScore $ findLeastScorePath matTest3 (matrixSortArg matTest3) DH.empty) "203"
  ]

testAll :: IO ()
testAll = do
  testPointsNext
  testLeastScore

{-
readCoord :: String -> (Int, Int)
readCoord = (\[d, e] -> (TR.read d, TR.read e)) . DLS.splitOn ","

answer :: IO ()
answer = do
  mass <- fmap (take 1024) $ getFileLines "day-18.txt"
  let blocks = map readCoord mass
      matx = map
              (\y ->
                map
                  (\x ->
                  if x == 70 && y == 70
                    then Exit
                    else if (x, y) `elem` blocks then Block else Free
                  )
                  [0..70]
              )
              [0..70]
  print $ fmap totalScore $ findLeastScorePath matx (matrixSortArg matx) DH.empty


answer2 :: IO ()
answer2 = do
  mass <- getFileLines "day-18.txt"
  let coords = map readCoord mass
  -- let analysedRange = ([1025..1045]::[Int])
  -- let analysedRange = ([1560..1570]::[Int])
  -- let analysedRange = ([2400..2405]::[Int])
  let analysedRange = ([2850..2900]::[Int])
  mapM_ putStrLn $ map (\n -> show n ++ " -> " ++ show (_solve n coords)) analysedRange
  print (mass !! 2876)
  where _solve :: Int -> [(Int, Int)] -> Maybe Int
        _solve n blocks =
          let limitedBlocks :: [(Int, Int)]
              limitedBlocks = take n blocks
              matx :: [[MapSlot]]
              matx = map
                      (\y ->
                        map
                          (\x ->
                          if x == 70 && y == 70
                            then Exit
                            else if (x, y) `elem` limitedBlocks then Block else Free
                          )
                          [0..70]
                      )
                      [0..70] 
          in fmap totalScore $ findLeastScorePath matx (matrixSortArg matx) DH.empty
    -}
