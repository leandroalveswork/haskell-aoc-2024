module Day15 (testAll, testAll2) where
--module Day15 (testAll, answer, testAll2, answer2) where

import qualified Data.List as DL
import qualified Data.Maybe as DM

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

itemAt :: (Int, Int) -> Matrix a -> Maybe a
itemAt (x, y) (Matrix hs) = fmap (\(_, _, e) -> e) (DL.find (\(x', y', _) -> x == x' && y == y') hs)

xlen :: Matrix a -> Int
xlen (Matrix hs) = maximum (map (\(x, _, _) -> x) hs) + 1

ylen :: Matrix a -> Int
ylen (Matrix hs) = maximum (map (\(_, y, _) -> y) hs) + 1

data Direction = Up | Right_ | Down | Left_
    deriving Eq

moveFwd :: Direction -> (Int, Int) -> (Int, Int)
moveFwd Up     (x,y) = (x    ,y - 1)
moveFwd Right_ (x,y) = (x + 1,y    )
moveFwd Down   (x,y) = (x    ,y + 1)
moveFwd Left_  (x,y) = (x - 1,y    )

fromChar :: Char -> Maybe Direction
fromChar '^' = Just Up
fromChar '>' = Just Right_
fromChar 'v' = Just Down
fromChar '<' = Just Left_
fromChar _   = Nothing

data MatState = MatState { player :: (Int, Int) , boxes :: [(Int, Int)] }

moveMatrix :: Direction -> MatState -> Matrix Bool -> (Matrix Bool, MatState)
moveMatrix d matS (Matrix hs) = let front :: (Int, Int)
                                    front = moveFwd d (player matS)
                                in case itemAt front (Matrix hs) of
                                        Nothing    -> (Matrix hs, matS)
                                        Just True  -> (Matrix hs, matS)
                                        Just False -> if front `notElem` boxes matS
                                                         then (Matrix hs, matS { player = front })
                                                         else let possibleFronts :: [(Int, Int)]
                                                                  possibleFronts = case d of
                                                                                        Up     -> [(fst front, y) | y <- [(snd front),(snd front - 1)..0]]
                                                                                        Right_ -> [(x, snd front) | x <- [(fst front)..(xlen (Matrix hs) - 1)]]
                                                                                        Down   -> [(fst front, y) | y <- [(snd front)..(ylen (Matrix hs) - 1)]]
                                                                                        Left_  -> [(x, snd front) | x <- [(fst front),(fst front - 1)..0]]
                                                                  boxesLifted :: Maybe ([(Int, Int)], Bool)
                                                                  boxesLifted = foldl
                                                                        (\monad p -> monad >>= (\(fts, lk) -> if lk
                                                                                                                 then Just (fts, lk)
                                                                                                                 else if DM.fromMaybe True (itemAt (moveFwd d p) (Matrix hs))
                                                                                                                 then Nothing
                                                                                                                 else if moveFwd d p `notElem` boxes matS
                                                                                                                 then Just (p:fts, True)
                                                                                                                 else Just (p:fts, False))
                                                                        )
                                                                        (Just ([], False))
                                                                        possibleFronts
                                                              in case boxesLifted of
                                                                         Nothing       -> (Matrix hs, matS)
                                                                         Just (bxs, _) -> let changedBxs :: [(Int, Int)]
                                                                                              changedBxs = (++ map (moveFwd d) bxs) $ filter (`notElem` bxs) $ boxes matS
                                                                                          in (Matrix hs, matS { player = front , boxes = changedBxs })

smallMatrix1 :: [String]
smallMatrix1 =
    [ "########"
    , "#..O.O.#"
    , "##@.O..#"
    , "#...O..#"
    , "#.#.O..#"
    , "#...O..#"
    , "#......#"
    , "########"
    ]

trimWalls :: [String] -> [String]
trimWalls = map (init . tail) . init . tail

-- too much work to setup tests, compare list of string instead
readWholeMatrix :: [String] -> (Matrix Bool, MatState)
readWholeMatrix lns = let (Matrix hs) = toMatrix lns
                      in foldl
                            (\(Matrix accHs, matS) (x, y, c) -> case c of
                                                                     '@' -> (Matrix ((x, y, False):accHs), matS { player = (x, y) })
                                                                     '#' -> (Matrix ((x, y, True ):accHs), matS)
                                                                     'O' -> (Matrix ((x, y, False):accHs), matS { boxes = (x, y):boxes matS })
                                                                     _   -> (Matrix ((x, y, False):accHs), matS)
                            )
                            (Matrix [], MatState { player = (0, 0) , boxes = [] })
                            hs

displayWholeMatrix :: Bool -> (Matrix Bool, MatState) -> [String]
displayWholeMatrix useDoubles (m, matS) = map
                                            (\y -> foldMap
                                                    (\x -> case itemAt (x, y) m of
                                                                Nothing    -> "#"
                                                                Just True  -> "#"
                                                                Just False -> if player matS == (x, y)
                                                                                then "@"
                                                                                else if (not useDoubles) && (x, y) `elem` boxes matS
                                                                                then "O"
                                                                                else if useDoubles && (x, y) `elem` boxes matS
                                                                                then "["
                                                                                else if useDoubles && (x - 1, y) `elem` boxes matS
                                                                                then "]"
                                                                                else "."
                                                    )
                                                    [0..(xlen m - 1)])
                                            [0..(ylen m - 1)]

smallMatrix2 :: [String]
smallMatrix2 =
    [ "########"
    , "#.@O.O.#"
    , "##..O..#"
    , "#...O..#"
    , "#.#.O..#"
    , "#...O..#"
    , "#......#"
    , "########"
    ]
smallMatrix3 :: [String]
smallMatrix3 =
    [ "########"
    , "#..@OO.#"
    , "##..O..#"
    , "#...O..#"
    , "#.#.O..#"
    , "#...O..#"
    , "#......#"
    , "########"
    ]
smallMatrix4 :: [String]
smallMatrix4 =
    [ "########"
    , "#...@OO#"
    , "##..O..#"
    , "#...O..#"
    , "#.#.O..#"
    , "#...O..#"
    , "#......#"
    , "########"
    ]
smallMatrix5 :: [String]
smallMatrix5 =
    [ "########"
    , "#....OO#"
    , "##.....#"
    , "#.....O#"
    , "#.#O@..#"
    , "#...O..#"
    , "#...O..#"
    , "########"
    ]

directions1 :: [Direction]
directions1 = foldMap (map (DM.fromMaybe Up) . filter DM.isJust . map fromChar) (filter ((>=1).length) ["<^^>>>vv<v>>v<"])

afterDirs1 :: (Matrix Bool, MatState)
afterDirs1 = foldl (\(m', matS') d -> moveMatrix d matS' m') (readWholeMatrix (trimWalls smallMatrix1)) directions1

moveMatrixComps :: [Comparation [String]]
moveMatrixComps =
    [ Comparation (101, trimWalls smallMatrix1, rightAdjoint (\(m, matS) -> moveMatrix Left_ matS m) (trimWalls smallMatrix1))
    , Comparation (102, trimWalls smallMatrix2, rightAdjoint (\(m, matS) -> moveMatrix Up matS m) (trimWalls smallMatrix1))
    , Comparation (103, trimWalls smallMatrix2, rightAdjoint (\(m, matS) -> moveMatrix Up matS m) (trimWalls smallMatrix2))
    , Comparation (104, trimWalls smallMatrix3, rightAdjoint (\(m, matS) -> moveMatrix Right_ matS m) (trimWalls smallMatrix2))
    , Comparation (105, trimWalls smallMatrix4, rightAdjoint (\(m, matS) -> moveMatrix Right_ matS m) (trimWalls smallMatrix3))
    , Comparation (106, trimWalls smallMatrix4, rightAdjoint (\(m, matS) -> moveMatrix Right_ matS m) (trimWalls smallMatrix4))
    , Comparation (107, trimWalls smallMatrix5, displayWholeMatrix False afterDirs1)
    ]
    where rightAdjoint :: ((Matrix Bool, MatState) -> (Matrix Bool, MatState)) -> [String] -> [String]
          rightAdjoint f = displayWholeMatrix False . f . readWholeMatrix

gpsScore :: (Int, Int) -> Integer
gpsScore (x, y) = toInteger ((x + 1) + 100 * (y + 1))

fullGpsScore :: (Matrix Bool, MatState) -> [Direction] -> Integer
fullGpsScore (m, matS) ds = let afterDs :: (Matrix Bool, MatState)
                                afterDs = foldl (\(m', matS') d -> moveMatrix d matS' m') (m, matS) ds
                            in sum (map gpsScore (boxes (snd afterDs)))

testAll :: IO ()
testAll = (putStrLn . unlines . map show) moveMatrixComps

{-
answer :: IO ()
answer = do
    mass <- getFileLines "day-15.txt"
    (let massMatrix :: (Matrix Bool, MatState)
         massMatrix = readWholeMatrix $ trimWalls $ take massSplit mass
         allDirections :: [Direction]
         allDirections = foldMap (map (DM.fromMaybe Up) . filter DM.isJust . map fromChar) ((filter ((>=1).length) . drop massSplit) mass)
     in print (fullGpsScore massMatrix allDirections))
    where massSplit :: Int
          massSplit = 50
          -}

noConflictLargeBoxes :: [(Int, Int)] -> (Int, Int) -> Bool
noConflictLargeBoxes boxes p = p `notElem` boxes && (fst p - 1, snd p) `notElem` boxes

tryLiftLargeBox :: Direction -> [(Int, Int)] -> Matrix Bool -> (Int, Int) -> Maybe [(Int, Int)]
tryLiftLargeBox d boxes (Matrix hs) p = if DM.fromMaybe True (itemAt (moveFwd d p) (Matrix hs)) || DM.fromMaybe True (itemAt (moveFwd d (fst p + 1, snd p)) (Matrix hs))
                                           then Nothing
                                           else let candidates :: [(Int, Int)]
                                                    candidates = [(if p2 `notElem` boxes then (fst p2 - 1, snd p2) else p2)
                                                                        | p2 <- [moveFwd d p, moveFwd d (fst p + 1, snd p)]
                                                                        , not (noConflictLargeBoxes boxes p2)]
                                                in fmap (p:) (foldl
                                                                (\acc candt -> do
                                                                                fts <- acc
                                                                                nextFts <- tryLiftLargeBox d (filter (/=candt) boxes) (Matrix hs) candt
                                                                                return (fts ++ nextFts)
                                                                )
                                                                (Just [])
                                                                candidates
                                                             )

moveMatrixLarge :: Direction -> MatState -> Matrix Bool -> (Matrix Bool, MatState)
moveMatrixLarge d matS (Matrix hs) = let front :: (Int, Int)
                                         front = moveFwd d (player matS)
                                     in case itemAt front (Matrix hs) of
                                             Nothing    -> (Matrix hs, matS)
                                             Just True  -> (Matrix hs, matS)
                                             Just False -> if noConflictLargeBoxes (boxes matS) front
                                                              then (Matrix hs, matS { player = front })
                                                              else let boxesLifted :: Maybe [(Int, Int)]
                                                                       boxesLifted = if front `notElem` boxes matS
                                                                                        then tryLiftLargeBox d (filter (/=(fst front - 1, snd front)) (boxes matS)) (Matrix hs) (fst front - 1, snd front)
                                                                                        else tryLiftLargeBox d (filter (/=front) (boxes matS)) (Matrix hs) front
                                                                   in case boxesLifted of
                                                                           Nothing  -> (Matrix hs, matS)
                                                                           Just bxs -> let changedBxs :: [(Int, Int)]
                                                                                           changedBxs = (++ map (moveFwd d) bxs) $ filter (`notElem` bxs) $ boxes matS
                                                                                       in (Matrix hs, matS { player = front , boxes = changedBxs })

doubleMatrix :: (Matrix Bool, MatState) -> (Matrix Bool, MatState)
doubleMatrix (Matrix hs, matS) =
    ( Matrix (concatMap (\(x, y, e) -> [(2 * x, y, e), (2 * x + 1, y, e)]) hs)
    , matS { player = (fst (player matS) * 2, snd (player matS)) , boxes = map (\(bx, by) -> (bx * 2, by)) (boxes matS) }
    )

smallMatrixT :: [String]
smallMatrixT =
    [ "##########"
    , "#..O..O.O#"
    , "#......O.#"
    , "#.OO..O.O#"
    , "#..O@..O.#"
    , "#O#..O...#"
    , "#O..O..O.#"
    , "#.OO.O.OO#"
    , "#....O...#"
    , "##########"
    ]

largeMatrixT1 :: [String]
largeMatrixT1 =
    [ "....[]....[]..[]"
    , "............[].."
    , "..[][]....[]..[]"
    , "....[]@.....[].."
    , "[]##....[]......"
    , "[]....[]....[].."
    , "..[][]..[]..[][]"
    , "........[]......"
    ]

largeMatrixT2 :: [String]
largeMatrixT2 =
    [ "[].......[].[][]"
    , "[]...........[]."
    , "[]........[][][]"
    , "[]......[]....[]"
    , "..##......[]...."
    , "..[]............"
    , "..@......[].[][]"
    , "......[][]..[].."
    ]

directionsT :: [Direction]
directionsT = foldMap
                (map (DM.fromMaybe Up) . filter DM.isJust . map fromChar)
                [ "<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^"
                , "vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v"
                , "><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<"
                , "<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^"
                , "^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><"
                , "^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^"
                , ">^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^"
                , "<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>"
                , "^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>"
                , "v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^"
                ]

foldDirections :: [Direction] -> (Matrix Bool, MatState) -> (Matrix Bool, MatState)
foldDirections ds (m, matS) = foldl (\(m', matS') d -> moveMatrixLarge d matS' m') (m, matS) ds

doubleMatrixComps :: [Comparation [String]]
doubleMatrixComps =
    [ Comparation (1001, largeMatrixT1, (displayWholeMatrix True . doubleMatrix . readWholeMatrix . trimWalls) smallMatrixT)
    , Comparation (1002, largeMatrixT2, rightAdjoint (foldDirections directionsT) (trimWalls smallMatrixT))
    ]
    where rightAdjoint :: ((Matrix Bool, MatState) -> (Matrix Bool, MatState)) -> [String] -> [String]
          rightAdjoint f = displayWholeMatrix True . f . doubleMatrix . readWholeMatrix

gpsScoreLarge :: (Int, Int) -> Integer
gpsScoreLarge (x, y) = toInteger (x + 2 + 100 * (y + 1))

fullGpsScoreLarge :: (Matrix Bool, MatState) -> [Direction] -> Integer
fullGpsScoreLarge (m, matS) ds = let afterDs :: (Matrix Bool, MatState)
                                     afterDs = foldDirections ds (m, matS)
                                 in sum (map gpsScoreLarge (DL.nub (boxes (snd afterDs))))

                                 {-
fullGpsScoreLargeDebug :: (Matrix Bool, MatState) -> [Direction] -> DWriter Integer
fullGpsScoreLargeDebug (m, matS) ds = let afterDs :: (Matrix Bool, MatState)
                                          afterDs = foldDirections ds (m, matS)
                                      in do
                                            debug (boxes (snd afterDs))
                                            return (sum (map (gpsScoreLarge (xlen m + 1, ylen m)) (boxes (snd afterDs))))
                                            -}

fullGpsScoreLargeComps :: [Comparation Integer]
fullGpsScoreLargeComps =
    [ Comparation (1301, 9021, ((\x -> fullGpsScoreLarge x directionsT) . doubleMatrix . readWholeMatrix . trimWalls) smallMatrixT )
    ]

testAll2 :: IO ()
testAll2 = do
    (putStrLn . unlines . map show) doubleMatrixComps
    (putStrLn . unlines . map show) fullGpsScoreLargeComps
    --putStrLn (getWritten (((\x -> fullGpsScoreLargeDebug x directionsT) . doubleMatrix . readWholeMatrix . trimWalls) smallMatrixT))

    {-
answer2 :: IO ()
answer2 = do
    mass <- getFileLines "day-15.txt"
    (let massMatrix :: (Matrix Bool, MatState)
         massMatrix = doubleMatrix $ readWholeMatrix $ trimWalls $ take massSplit mass
         allDirections :: [Direction]
         allDirections = foldMap (map (DM.fromMaybe Up) . filter DM.isJust . map fromChar) ((filter ((>=1).length) . drop massSplit) mass)
     in print (fullGpsScoreLarge massMatrix allDirections))
    where massSplit :: Int
          massSplit = 50
          -}
