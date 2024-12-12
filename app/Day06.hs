module Day06 (testAll, testAll2) where
--module Day06 (testAll, answer, testAll2, answer2) where

import qualified Data.List.Split as DLS
import qualified Data.Maybe as DM
import qualified Data.List as DL

import TestLib (Comparation(Comparation))
import DataMassParsers (getFileLines)

newtype Point = Point (Int, Int)
    deriving Eq

data Direction = Up | Right_ | Down | Left_
    deriving Eq

turn :: Direction -> Direction
turn Up     = Right_
turn Right_ = Down
turn Down   = Left_
turn Left_   = Up

moveFwd :: Direction -> Point -> Point
moveFwd Up     (Point (x,y)) = Point (x    ,y - 1)
moveFwd Right_ (Point (x,y)) = Point (x + 1,y    )
moveFwd Down   (Point (x,y)) = Point (x    ,y + 1)
moveFwd Left_  (Point (x,y)) = Point (x - 1,y    )

moveUntilOutside :: Point -> Direction -> [Point] -> (Int, Int) -> (Point, [Point])
moveUntilOutside (Point (x, y)) dir obstacs (maxx, maxy)
    | x < 0 || y < 0 || x > maxx || y > maxy = (Point (x, y), [])
    | otherwise                              = let (res, allPs) = if moveFwd dir (Point (x, y)) `elem` obstacs
                                                                     then moveUntilOutside (Point (x, y)) (turn dir) obstacs (maxx, maxy)
                                                                     else moveUntilOutside (moveFwd dir (Point (x, y))) dir obstacs (maxx, maxy)
                                               in (res, Point (x,y):allPs)

fromLines :: [String] -> Maybe (Point, Direction, [Point], (Int, Int))
fromLines lns = let zippedPoses = concat (zipWith
                                                (\j1 line -> zipWith (\i1 e -> (i1, j1, e) ) [0..] line)
                                                [0..]
                                                lns)
                    pt = fmap (\(x, y, _) -> Point (x, y)) $ DL.find (\(x, y, c) -> c == '^') zippedPoses
                    dr = fmap (const Up) $ DL.find (\(x, y, c) -> c == '^') zippedPoses
                    maxx = maximum $ map (\(x, y, c) -> x) zippedPoses
                    maxy = maximum $ map (\(x, y, c) -> y) zippedPoses
                    obstacs = map (\(x, y, _) -> Point (x, y)) $ filter (\(x, y, c) -> c == '#') zippedPoses
                in do
                    okPt <- pt
                    okDr <- dr
                    return (okPt, okDr, obstacs, (maxx, maxy))

lns1 :: Maybe (Point, Direction, [Point], (Int, Int))
lns1 = fromLines
    [ ".#.."
    , "...#"
    , ".^.#"
    ]
lns2 :: Maybe (Point, Direction, [Point], (Int, Int))
lns2 = fromLines
    [ "...."
    , "#.##"
    , "#^.#"
    ]
lns3 :: Maybe (Point, Direction, [Point], (Int, Int))
lns3 = fromLines
    [ "....#....."
    , ".........#"
    , ".........."
    , "..#......."
    , ".......#.."
    , ".........."
    , ".#..^....."
    , "........#."
    , "#........."
    , "......#..."
    ]

positionsPassed :: [Point] -> Int
positionsPassed = length . DL.nub

moveUntilComps :: [Comparation Int]
moveUntilComps =
    [ Comparation (1, 4, (positionsPassed . snd . runMoves . (\(Just x) -> x)) lns1)
    , Comparation (2, 3, (positionsPassed . snd . runMoves . (\(Just x) -> x)) lns2)
    , Comparation (3, 41, (positionsPassed . snd . runMoves . (\(Just x) -> x)) lns3)
    ]
    where runMoves :: (Point, Direction, [Point], (Int, Int)) -> (Point, [Point])
          runMoves (pt, dr, obstacs, limits) = moveUntilOutside pt dr obstacs limits

testAll :: IO ()
testAll = (putStrLn . unlines . map show) moveUntilComps

{-
answer :: IO ()
answer = do
    mass <- getFileLines "day-06.txt"
    print $ sumPositions $ fromLines $ filter ((>=1).length) mass
    where sumPositions :: Maybe (Point, Direction, [Point], (Int, Int)) -> Int
          sumPositions = positionsPassed . snd . runMoves . (\(Just x) -> x)
          runMoves :: (Point, Direction, [Point], (Int, Int)) -> (Point, [Point])
          runMoves (pt, dr, obstacs, limits) = moveUntilOutside pt dr obstacs limits
          -}

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

fasterMoveUntilOutside :: [(Point, Direction)] -> Point -> Direction -> [Point] -> (Int, Int) -> Maybe (Point, [(Point, Direction)])
fasterMoveUntilOutside alreadies (Point (x, y)) dir obstacs (maxx, maxy)
    | x < 0 || y < 0 || x > maxx || y > maxy = Just (Point (x, y), alreadies)
    | otherwise                              = let nextHouse :: (Point, Direction)
                                                   nextHouse = case dir of
                                                                    Up     -> case (safeHead . DL.sortOn (\(Point (px, py)) -> -py) . filter (\(Point (px, py)) -> px == x && py < y)) obstacs of
                                                                                   Just (Point (ox, oy)) -> (Point (ox, oy + 1), Right_)
                                                                                   Nothing               -> (Point (x, -1), Up)
                                                                    Right_ -> case (safeHead . DL.sortOn (\(Point (px, py)) -> px) . filter (\(Point (px, py)) -> py == y && px > x)) obstacs of
                                                                                   Just (Point (ox, oy)) -> (Point (ox - 1, oy), Down)
                                                                                   Nothing               -> (Point (maxx + 1, y), Right_)
                                                                    Down   -> case (safeHead . DL.sortOn (\(Point (px, py)) -> py) . filter (\(Point (px, py)) -> px == x && py > y)) obstacs of
                                                                                   Just (Point (ox, oy)) -> (Point (ox, oy - 1), Left_)
                                                                                   Nothing               -> (Point (x, maxy + 1), Down)
                                                                    Left_  -> case (safeHead . DL.sortOn (\(Point (px, py)) -> -px) . filter (\(Point (px, py)) -> py == y && px < x)) obstacs of
                                                                                   Just (Point (ox, oy)) -> (Point (ox + 1, oy), Up)
                                                                                   Nothing               -> (Point (-1, y), Left_)
                                               in if nextHouse `elem` alreadies
                                                     then Nothing
                                                     else fasterMoveUntilOutside (nextHouse:alreadies) (fst nextHouse) (snd nextHouse) obstacs (maxx, maxy)

containsLoop :: Point -> Direction -> [Point] -> (Int, Int) -> Bool
containsLoop a b c d = DM.isNothing (fasterMoveUntilOutside [] a b c d)

moveAndReturnBlocks :: Point -> Direction -> [Point] -> (Int, Int) -> (Point, [Point])
moveAndReturnBlocks (Point (x, y)) dir obstacs (maxx, maxy) = let (fin, ps) = moveUntilOutside (Point (x, y)) dir obstacs (maxx, maxy)
                                                               in (fin, filter
                                                                            (\p -> containsLoop (Point (x, y)) dir (p:obstacs) (maxx, maxy)
                                                                                && p /= Point (x, y) )
                                                                            (DL.nub ps))

instance Show Point where
    show (Point (x, y)) = show (x, y)

lns4 :: Maybe (Point, Direction, [Point], (Int, Int))
lns4 = fromLines
    [ ".#..#......#...."
    , "...#.....#.#...."
    , "#............#.."
    , "..#.......#.#..."
    , ".......#........"
    , "..........#....."
    , ".#..^..........."
    , "........#......."
    , "#..............."
    , "......#........."
    ]
lns2Easy :: Maybe (Point, Direction, [Point], (Int, Int))
lns2Easy = fromLines
    [ ".#.."
    , "..##"
    , ".^.#"
    ]
lns3Block :: Maybe (Point, Direction, [Point], (Int, Int))
lns3Block = fromLines
    [ "....#....."
    , ".........#"
    , ".........."
    , "..#......."
    , ".......#.."
    , ".........."
    , ".#..^....."
    , "........#."
    , "##........"
    , "......#..."
    ]
lns4Block :: Maybe (Point, Direction, [Point], (Int, Int))
lns4Block = fromLines
    [ ".#..#......#...."
    , "...#.....#.#...."
    , "#............#.."
    , "..#.......#.#..."
    , ".......#........"
    , "..........#....."
    , ".#..^..........."
    , "..#.....#......."
    , "#..............."
    , "......#........."
    ]

moveBlockComps :: [Comparation [Point]]
moveBlockComps =
    [ Comparation (101, [Point (1, 8), Point (3, 6), Point (3, 8), Point (6, 7), Point (7, 7), Point (7, 9)]
        , (sortOnlyForTest . DL.nub . snd . runMoves . (\(Just x) -> x)) lns3)
    , Comparation (102, [Point (1, 8), Point (2, 6), Point (2, 8), Point (3, 6), Point (3, 8), Point (4, 1),
        Point (6, 7), Point (7, 7), Point (7, 9), Point (8, 1), Point (8, 2), Point (8, 3), Point (8, 4)]
        , (sortOnlyForTest . DL.nub . snd . runMoves . (\(Just x) -> x)) lns4)
    ]
    where runMoves :: (Point, Direction, [Point], (Int, Int)) -> (Point, [Point])
          runMoves (pt, dr, obstacs, limits) = moveAndReturnBlocks pt dr obstacs limits
          sortOnlyForTest :: [Point] -> [Point]
          sortOnlyForTest = DL.sortOn (\(Point (x, y)) -> 10000 * x + y)

moveFasterComps :: [Comparation Bool]
moveFasterComps =
    [ Comparation (131, False, (runMoves . (\(Just x) -> x)) lns1)
    , Comparation (132, False, (runMoves . (\(Just x) -> x)) lns2)
    , Comparation (133, False, (runMoves . (\(Just x) -> x)) lns3)
    , Comparation (134, False, (runMoves . (\(Just x) -> x)) lns2Easy)
    , Comparation (135, True, (runMoves . (\(Just x) -> x)) lns3Block)
    , Comparation (136, False, (runMoves . (\(Just x) -> x)) lns4Block)
    ]
    where runMoves :: (Point, Direction, [Point], (Int, Int)) -> Bool
          runMoves (pt, dr, obstacs, limits) = containsLoop pt dr obstacs limits

testAll2 :: IO ()
testAll2 = do
    (putStrLn . unlines . map show) moveBlockComps
    (putStrLn . unlines . map show) moveFasterComps

    {-
answer2 :: IO ()
answer2 = do
    mass <- getFileLines "day-06.txt"
    print $ sumPositions $ fromLines $ filter ((>=1).length) mass
    where sumPositions :: Maybe (Point, Direction, [Point], (Int, Int)) -> Int
          sumPositions = length . DL.nub . snd . runMoves . (\(Just x) -> x)
          runMoves :: (Point, Direction, [Point], (Int, Int)) -> (Point, [Point])
          runMoves (pt, dr, obstacs, limits) = moveAndReturnBlocks pt dr obstacs limits
          -}
