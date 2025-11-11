{-# LANGUAGE ScopedTypeVariables #-}

module Day16 (testAll, testAll2) where
-- module Day16 (testAll, answer, testAll2, answer2) where

import qualified Data.Maybe as DM
import qualified Data.List as DL
import qualified Data.List.Split as DLS

import TestLib (Comparation(Comparation))
import DataMassParsers (getFileLines)

data Direction = Up | Right_ | Down | Left_
  deriving (Eq, Show)

directionIndex :: Direction -> Int
directionIndex Up = 0
directionIndex Right_ = 1
directionIndex Down = 2
directionIndex Left_ = 3

instance Ord Direction where
  compare d d' = compare (directionIndex d) (directionIndex d')

turn :: Direction -> Direction
turn Up     = Right_
turn Right_ = Down
turn Down   = Left_
turn Left_  = Up

newtype Point = Point (Int, Int) deriving (Eq, Show)
instance Ord Point where
  (Point (x, y)) `compare` (Point (x2, y2))
    | x == x2 = y `compare` y2
    | otherwise = x `compare` x2


turnLeft :: Direction -> Direction
turnLeft = turn . turn . turn

moveFwd :: Direction -> Point -> Point
moveFwd Up     (Point (x,y)) = Point (x    ,y - 1)
moveFwd Right_ (Point (x,y)) = Point (x + 1,y    )
moveFwd Down   (Point (x,y)) = Point (x    ,y + 1)
moveFwd Left_  (Point (x,y)) = Point (x - 1,y    )

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

testSort :: IO ()
testSort = 
  let sort0 :: Sort Int
      sort0 = (pushSort 90 . pushSort 70 . pushSort 75 . pushSort 25 . pushSort 50) NoSort
      sort0Scratch :: Sort Int
      sort0Scratch = MakeSort
        (MakeSort NoSort 25 NoSort)
        50
        (MakeSort 
          (MakeSort NoSort 70 NoSort)
          75
          (MakeSort NoSort 90 NoSort)
        )
      sort1 :: Sort Int
      sort1 = (pushSort 20 
        . pushSort 30 
        . pushSort 75 
        . pushSort 25 
        . pushSort 50) NoSort
      sort1pop1 :: Sort Int
      sort1pop1 = (pushSort 30 . pushSort 75 . pushSort 25 . pushSort 50) NoSort
      sort1pop2 :: Sort Int
      sort1pop2 = (pushSort 30 . pushSort 75 . pushSort 50) NoSort
      inlineComps :: [Comparation (Sort Int)]
      inlineComps =
        [ Comparation (101, sort0Scratch, sort0)
        , Comparation (102, sort1pop1, (popSort sort1))
        , Comparation (103, sort1pop2, (popSort sort1pop1))
        ]
      elemComps :: [Comparation Bool]
      elemComps =
        [ Comparation (104, True, elemSort 70 sort0)
        , Comparation (105, False, elemSort 65 sort0)
        , Comparation (106, True, elemSort 25 sort0)
        , Comparation (107, True, elemSort 90 sort0)
        ]
      leastComps :: [Comparation (Maybe Int)]
      leastComps =
        [ Comparation (108, Nothing, leastOfSort NoSort)
        , Comparation (109, Just 20, leastOfSort sort1)
        , Comparation (110, Just 25, leastOfSort sort1pop1)
        , Comparation (111, Just 30, leastOfSort sort1pop2)
        ]
      stringComps :: [Comparation String]
      stringComps =
        [ Comparation (112, "-90-75-70-50-25", foldr (\x acc -> acc ++ "-" ++ show x) "" sort0)
        , Comparation (113, "3", show $ foldr (\_ acc -> acc + 1) 0 sort1pop2)
        , Comparation (114, "3", show $ length sort1pop2)
        ]
      onlyIfNotElemComps :: [Comparation Int]
      onlyIfNotElemComps =
        [ Comparation (115, 3, length $ foldr pushSortOnlyIfNotElem NoSort [20, 30, 40, 40])
        , Comparation (116, 1, length $ foldr pushSortOnlyIfNotElem NoSort [20, 20, 20, 20])
        , Comparation (117, 5, length $ foldr pushSortOnlyIfNotElem NoSort [40, 20, 30, 40, 50, 30, 10])
        ]
  in do
    (putStrLn . unlines . map show) inlineComps
    (putStrLn . unlines . map show) elemComps
    (putStrLn . unlines . map show) leastComps
    (putStrLn . unlines . map show) stringComps
    (putStrLn . unlines . map show) onlyIfNotElemComps 

data MapSlot = Start | Free | Wall | Exit deriving (Eq, Show)

runPoint :: Point -> (Int, Int)
runPoint (Point p) = p

data Path = Path
  { currentP :: Point
  , toP :: Point
  , previousPoints :: [(Point, Direction)]
  , previousSorted :: Sort Point
  , facingDirection :: Direction
  , totalScore :: Int
  , matrixBounds :: (Int, Int)
  }
  deriving (Eq)

showPath :: Path -> Bool -> String
showPath ph withPreviousSorted =
  let showDirec :: Direction -> Char
      showDirec Up = '^'
      showDirec Down = 'v'
      showDirec Left_ = '<'
      showDirec Right_ = '>'
      showFirstChar :: Int -> Int -> Char
      showFirstChar x y
        | any (\(p, _) -> runPoint p == (x, y)) (previousPoints ph) = 
          (showDirec 
            . (DM.fromMaybe Up)
            . (fmap snd)
            . (DL.find (\(p, _) -> runPoint p == (x, y)))
          )
            (previousPoints ph)
        | runPoint (toP ph) == (x, y) = '*'
        | runPoint (currentP ph) == (x, y) = showDirec (facingDirection ph)
        | otherwise = ' '
      showSecondChar :: Int -> Int -> Char
      showSecondChar x y = if elemSort (Point (x, y)) (previousSorted ph) then '+' else ' '
  in "{ Score = " ++ show (totalScore ph)
        ++ concatMap
             (\y -> "\n  "
                      ++ (concatMap
                            (\x -> showFirstChar x y : [c | c <- [showSecondChar x y], withPreviousSorted])
                            [0..fst (matrixBounds ph)]
                          )
              )
             [0..snd (matrixBounds ph)]
        ++ "}"

instance Show Path where
  show ph = showPath ph True

newtype ScoredPath = ScoredPath Path

instance Show ScoredPath where
  show (ScoredPath scored) = "[scored]-> " ++ show scored

instance Eq ScoredPath where
  (ScoredPath a) == (ScoredPath b) = (totalScore a) == (totalScore b)
instance Ord ScoredPath where
  compare (ScoredPath a) (ScoredPath b) = compare (totalScore a) (totalScore b)


newtype PointedPath = PointedPath Path

instance Show PointedPath where
  show (PointedPath pointed) = "[pointed]-> " ++ show pointed

instance Eq PointedPath where
  (PointedPath a) == (PointedPath b) =
    ((currentP a) == (currentP b)) && ((facingDirection a) == (facingDirection b))
instance Ord PointedPath where
  (PointedPath a) `compare` (PointedPath b)
    | (currentP a) == (currentP b) = (facingDirection a) `compare` (facingDirection b)
    | otherwise = (currentP a) `compare` (currentP b)

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

matTest1 :: [[MapSlot]]
matTest1 =
  [ [Free, Free, Free, Wall, Exit]
  , [Free, Wall, Free, Free, Free]
  , [Free, Wall, Wall, Wall, Free]
  , [Free, Wall, Wall, Free, Free]
  , [Start, Free, Free, Free, Wall]
  ]

testPointsNext :: IO ()
testPointsNext =
  let pointsComps :: [Comparation [(Int, Int)]]
      pointsComps =
        [ Comparation (201, [(1, 4), (0, 3)], pointsNext matTest1 (0, 4))
        , Comparation (202, [(2, 0), (0, 0)], pointsNext matTest1 (1, 0))
        , Comparation (203, [(4, 3), (3, 4)], pointsNext matTest1 (3, 3))
        , Comparation (204, [(3, 1), (4, 2), (4, 0)], pointsNext matTest1 (4, 1))
        ]
  in (putStrLn . unlines . map show) pointsComps

pointsNext_ :: [[MapSlot]] -> Point -> [Point]
pointsNext_ matx p = map Point $ pointsNext matx (runPoint p)

findPaths :: [[MapSlot]] -> Path -> [Path]
findPaths matx ph =
  let backwards :: Point
      backwards = moveFwd ((turn . turn) $ facingDirection ph) $ currentP ph
      pointsNextToPh :: [Point]
      pointsNextToPh = filter (\x -> not (elemSort x (previousSorted ph))) 
                          $ filter ((/=) backwards) 
                          $ pointsNext_ matx (currentP ph)

      pointsCanMoveTo :: [(Point, Direction, Int)]
      pointsCanMoveTo =
        [e
          | e <-
            [ (moveFwd (turnLeft (facingDirection ph)) $ currentP ph, turnLeft (facingDirection ph), 1001)
            , (moveFwd (turn (facingDirection ph)) $ currentP ph, turn (facingDirection ph), 1001)
            , (moveFwd (facingDirection ph) $ currentP ph, facingDirection ph, 1)
            ]
          , ((\(t, _, _) -> t) e) `elem` pointsNextToPh
        ]
  in map
          (\(p, direc, increment) -> Path
            { currentP = p
            , toP = toP ph
            , previousPoints = (currentP ph, facingDirection ph):(previousPoints ph)
            , previousSorted = pushSort (currentP ph) (previousSorted ph)
            , facingDirection = direc
            , totalScore = totalScore ph + increment
            , matrixBounds = matrixBounds ph
            })
          pointsCanMoveTo

findAllPaths :: [[MapSlot]] -> Path -> [Path]
findAllPaths matx ph = 
  let foundPhs = findPaths matx ph
      atExit :: Path -> Bool
      atExit ph2 = toP ph2 == currentP ph2
  in (filter atExit foundPhs)
       ++ concatMap (findAllPaths matx) (filter (not . atExit) foundPhs)



findLeastScorePath :: [[MapSlot]] -> Sort ScoredPath -> Sort PointedPath -> Maybe Path
findLeastScorePath matx phs pointedPhs =
  do
    wrappedLeastPh <- leastOfSort phs
    let remainingPhs :: Sort ScoredPath
        remainingPhs = popSort phs
        (ScoredPath leastPh) = wrappedLeastPh
        pathsFound ::[Path]
        pathsFound = findPaths matx leastPh
        pathCostsMore :: Path -> Sort PointedPath -> Bool
        pathCostsMore ph' kPhs = 
          case findInSort (PointedPath ph') kPhs of
                Nothing  -> False
                Just wrappedKPh ->
                  let (PointedPath kPh) = wrappedKPh
                  in case compare (totalScore ph') (totalScore kPh) of
                            LT -> False
                            GT -> True
                            EQ -> ph' /= kPh
        totalPointeds :: Sort PointedPath
        totalPointeds = foldl (\acc x -> if pathCostsMore x acc then acc else pushSort (PointedPath x) acc) pointedPhs pathsFound
      in case DL.find (\ph' -> toP ph' == currentP ph') pathsFound of
              Just phExit -> Just phExit
              Nothing -> 
                findLeastScorePath 
                  matx 
                  (foldl (\acc x -> if pathCostsMore x totalPointeds then acc else pushSort (ScoredPath x) acc) remainingPhs pathsFound)
                  totalPointeds

matTest2 :: [[MapSlot]]
matTest2 =
  [ [Wall, Free, Free, Free, Wall, Free, Exit]
  , [Free, Free, Wall, Free, Wall, Free, Wall]
  , [Free, Wall, Free, Free, Wall, Free, Wall]
  , [Free, Free, Free, Wall, Free, Free, Free]
  , [Free, Wall, Free, Free, Free, Wall, Wall]
  , [Free, Wall, Free, Wall, Wall, Wall, Free]
  , [Start, Free, Free, Free, Free, Free, Free]
  ]

testPathFind :: IO ()
testPathFind =
  let endP :: Point
      endP = Point (4, 0)
      previous1 :: [(Point, Direction)]
      previous1 =
        [ (Point (4, 1), Right_)
        , (Point (3, 1), Right_)
        , (Point (2, 1), Down)
        , (Point (2, 0), Right_)
        , (Point (1, 0), Right_)
        , (Point (0, 0), Up)
        , (Point (0, 1), Up)
        , (Point (0, 2), Up)
        , (Point (0, 3), Up)
        , (Point (0, 4), Right_)
        ]
      previous2 :: [(Point, Direction)]
      previous2 =
        [ (Point (4, 1), Up)
        , (Point (4, 2), Up)
        , (Point (4, 3), Right_)
        , (Point (3, 3), Up)
        , (Point (3, 4), Right_)
        , (Point (2, 4), Right_)
        , (Point (1, 4), Right_)
        , (Point (0, 4), Right_)
        ]
      inlineComps :: [Comparation [Path]]
      inlineComps =
        [ Comparation
          (301 
            , [ Path { currentP = endP, toP = endP, previousPoints = previous1, previousSorted = foldl (\acc x -> pushSort (fst x) acc) NoSort (reverse previous1), facingDirection = Up, totalScore = 5010, matrixBounds = (4, 4) }
              , Path { currentP = endP, toP = endP, previousPoints = previous2, previousSorted = foldl (\acc x -> pushSort (fst x) acc) NoSort (reverse previous2), facingDirection = Up, totalScore = 3008, matrixBounds = (4, 4) }
              ]
            , findAllPaths matTest1 $ Path { currentP = Point (0, 4), toP = endP, previousPoints = [], previousSorted = NoSort, facingDirection = Right_, totalScore = 0, matrixBounds = (4, 4)}
          )
        ]

      leastScoreComps :: [Comparation Int]
      leastScoreComps =
        [ Comparation
          (302
            , 6012
            , totalScore
                $ DL.minimumBy (\a b -> compare (totalScore a) (totalScore b)) 
                $ (findAllPaths matTest2 $ Path { currentP = Point (0, 6), toP = Point (6, 0), previousPoints = [], previousSorted = NoSort, facingDirection = Right_, totalScore = 0, matrixBounds = (6, 6)}))
        , Comparation
          (303, 6012, DM.fromMaybe 0
                        $ fmap totalScore 
                        $ (\scoredPh -> findLeastScorePath matTest2 scoredPh NoSort)
                        $ (\e -> MakeSort NoSort e NoSort)
                        $ ScoredPath
                        $ Path { currentP = Point (0, 6), toP = Point (6, 0), previousPoints = [], previousSorted = NoSort, facingDirection = Right_, totalScore = 0, matrixBounds = (6, 6)})
        , Comparation
          (304, 3008, DM.fromMaybe 0
                        $ fmap totalScore 
                        $ (\scoredPh -> findLeastScorePath matTest1 scoredPh NoSort)
                        $ (\e -> MakeSort NoSort e NoSort)
                        $ ScoredPath
                        $ Path { currentP = Point (0, 4), toP = Point (4, 0), previousPoints = [], previousSorted = NoSort, facingDirection = Right_, totalScore = 0, matrixBounds = (4, 4)})
        ]
  in do
    (putStrLn . unlines . map show) inlineComps 
    (putStrLn . unlines . map show) leastScoreComps

readWholeMatrix :: [String] -> ([[MapSlot]], Path)
readWholeMatrix lns =
  let slots :: [[MapSlot]]
      slots = 
        map
          (map (\c -> 
              case c of
                    'S' -> Start
                    'E' -> Exit
                    '#' -> Wall
                    '.' -> Free
                    _ -> Wall))
          lns
      matxBounds :: (Int, Int)
      matxBounds = (length (lns !! 0) - 1, length lns - 1)
  in (slots, Path { currentP = Point (0, snd matxBounds), toP = Point (fst matxBounds, 0), previousPoints = [], previousSorted = NoSort, facingDirection = Right_, totalScore = 0, matrixBounds = matxBounds })

testAll :: IO ()
testAll = do
  testSort
  testPointsNext
  testPathFind 

{-
answer :: IO ()
answer = do
  mass <- getFileLines "day-16.txt"
  (let (matx, ph) = readWholeMatrix mass
   in print 
        $ fmap totalScore 
        $ (\scoredPh -> findLeastScorePath matx scoredPh NoSort)
        $ (MakeSort NoSort (ScoredPath ph) NoSort))
        -}

data Origins = Origins
  { origPath :: PointedPath
  , origins :: [(Point, Direction)]
  }

instance Show Origins where
  show o = "origPath->" ++ show (origPath o)
    ++ "\norigins->" ++ show (origins o) 

pathFromPointedPoint :: Point -> Direction -> Path
pathFromPointedPoint pt direc =
  Path
    { currentP = pt
    , toP = Point (0, 0)
    , previousPoints = []
    , previousSorted = NoSort
    , facingDirection = direc
    , totalScore = 0
    , matrixBounds = (0, 0)
    }

instance Eq Origins where
  a == b = origPath a == origPath b

instance Ord Origins where
  compare a b = compare (origPath a) (origPath b)

generateFullOrigins :: (Point, Direction) -> Sort Origins -> Sort PointedPath
generateFullOrigins pt origs =
  let originPt :: Origins
      originPt =
        ( (\p -> Origins { origPath = p , origins = [] })
        . PointedPath
        . (uncurry pathFromPointedPoint)
        ) pt
      foundSorts :: [Origins]
      foundSorts = findManyInSort originPt origs
      generations :: [Sort PointedPath]
      generations =
        map
          (\s ->
            foldr
              (\u acc -> let newU = generateFullOrigins u origs
                         in foldr pushSortOnlyIfNotElem acc newU)
              NoSort
              (origins s)
          )
          foundSorts
    in foldr
        (foldr pushSortOnlyIfNotElem) 
        (MakeSort NoSort (origPath originPt) NoSort)
        generations

findTrail :: [[MapSlot]] 
  -> Sort ScoredPath 
  -> Sort PointedPath 
  -> Sort Origins 
  -> Maybe (Path, Sort PointedPath)
findTrail matx phs pointedPhs origs =
  do
    wrappedLeastPh <- leastOfSort phs
    let remainingPhs :: Sort ScoredPath
        remainingPhs = popSort phs
        (ScoredPath leastPh) = wrappedLeastPh
        pathsFound :: [Path]
        pathsFound = findPaths matx leastPh
        comparePathCosts :: Path -> Sort PointedPath -> Ordering
        comparePathCosts ph' kPhs =
          case findInSort (PointedPath ph') kPhs of
                Nothing -> LT
                Just wrappedKPh ->
                  let (PointedPath kPh) = wrappedKPh
                  in case compare (totalScore ph') (totalScore kPh) of
                            LT -> LT
                            GT -> GT
                            EQ -> if ph' /= kPh then GT else EQ

        totalPointeds :: Sort PointedPath
        totalPointeds = foldl (\acc x -> if comparePathCosts x acc == GT then acc else pushSort (PointedPath x) acc) pointedPhs pathsFound
        totalOrigs :: Sort Origins
        totalOrigs =
          foldr pushSortOnlyIfNotElem origs
            $ map (\(ph', (ScoredPath phFound)) -> Origins { origPath = PointedPath phFound, origins = previousPoints ph' })
            $ concatMap
                (\ph' ->
                  let phLike :: ScoredPath
                      phLike = ScoredPath $ ((pathFromPointedPoint (Point (0, 0)) Up) { totalScore = totalScore ph' })
                      phsSameScore :: [ScoredPath]
                      phsSameScore = findManyInSort phLike phs
                      phsSamePos :: [ScoredPath]
                      phsSamePos =
                        filter
                          (\(ScoredPath phSco) -> 
                              (currentP phSco == currentP ph') && (facingDirection phSco == facingDirection ph')
                          )
                          phsSameScore
                  in
                    map (\phFound -> (ph', phFound)) phsSamePos)
                pathsFound

      in case DL.find (\ph' -> toP ph' == currentP ph') pathsFound of
              Just phExit ->
                let originsAndExit :: Sort Origins
                    originsAndExit =
                      pushSort
                        (Origins { origPath = PointedPath phExit, origins = previousPoints phExit })
                        origs
                in
                  Just (
                    phExit
                    ,  generateFullOrigins (currentP phExit, facingDirection phExit) originsAndExit
                    )
              Nothing ->
                findTrail
                  matx
                  (foldl (\acc x -> if comparePathCosts x totalPointeds == GT then acc else pushSort (ScoredPath x) acc) remainingPhs pathsFound)
                  totalPointeds
                  totalOrigs

pointOccurrences :: Sort PointedPath -> Int
pointOccurrences =
  length
    . (foldr
        (\(PointedPath x) acc -> pushSortOnlyIfNotElem (currentP x) acc)
        NoSort)

matTest3 :: [[MapSlot]]
matTest3 =
  [ [Free, Free, Free, Free, Free, Free, Exit]
  , [Free, Wall, Free, Wall, Free, Wall, Wall]
  , [Free, Free, Free, Free, Free, Free, Free]
  , [Free, Wall, Free, Wall, Free, Free, Free]
  , [Free, Free, Free, Free, Free, Free, Free]
  , [Free, Wall, Free, Wall, Free, Free, Free]
  , [Start, Free, Free, Free, Free, Free, Free]
  ]
testFindTrail :: IO ()
testFindTrail =
  let countComps :: [Comparation (Maybe Int)]
      countComps =
        [ Comparation
          ( 1001
          , Just 27
          , fmap
              (pointOccurrences . snd)
              $ (\scoredPh -> findTrail matTest3 scoredPh NoSort NoSort)
              $ (\e -> MakeSort NoSort e NoSort)
              $ ScoredPath
              $ Path { currentP = Point (0, 6), toP = Point (6, 0), previousPoints = [], previousSorted = NoSort, facingDirection = Right_, totalScore = 0, matrixBounds = (6, 6)}
          ) 
        ]
  in (putStrLn . unlines . map show) countComps 


testAll2 :: IO ()
testAll2 = do
  testFindTrail

{-
answer2 :: IO ()
answer2 = do
  mass <- getFileLines "day-16.txt"
  (let (matx, ph) = readWholeMatrix mass
   in print 
        $ fmap (pointOccurrences . snd)
        $ (\scoredPh -> findTrail matx scoredPh NoSort NoSort)
        $ (MakeSort NoSort (ScoredPath ph) NoSort))
        -}
