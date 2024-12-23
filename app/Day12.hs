module Day12 (testAll, testAll2) where
--module Day12 (testAll, answer, testAll2, answer2) where

import qualified Data.List as DL
import qualified Data.Bifunctor as BF

import Debugging (DWriter(DWriter), debug, getWritten, tell)
import TestLib (Comparation(Comparation))
import DataMassParsers (getFileLines)

newtype Area = Area [(Int, Int)]
    deriving Eq
instance Show Area where
    show (Area xs) = "a:" ++ show xs

runTiles :: Area -> [(Int, Int)]
runTiles (Area xs) = xs

tilesAreNextEachOther :: (Int, Int) -> (Int, Int) -> Bool
tilesAreNextEachOther (x', y') (x, y)
    | x' == x   = abs (y' - y) == 1
    | y' == y   = abs (x' - x) == 1
    | otherwise = False

newtype Region = Region (Char, Area)
    deriving Eq

instance Show Region where
    show (Region (c, a)) = show c ++ "=>" ++ show a

perimet :: Area -> Int
perimet (Area ts) = (length ts * 4) - length (filter id (tilesAreNextEachOther <$> ts <*> ts))

cost :: Area -> Int
cost (Area ts) = perimet (Area ts) * length ts

costComps :: [Comparation Int]
costComps =
    [ Comparation (401, 40, cost (Area [(0, 0), (0, 1), (1, 0), (2, 0)]))
    , Comparation (402, 12, cost (Area [(1, 1), (2, 1)]))
    , Comparation (403, 84, cost (Area [(0, 0), (0, 1), (1, 0), (1, 1), (2, 0), (2, 1), (2, 2)]))
    , Comparation (404, 144, cost (Area [(0, 0), (2, 0), (3, 0), (4, 0), (0, 1), (1, 1), (2, 1), (4, 1)]))
    ]

fromRawTexts :: [String] -> [(Char, (Int, Int))]
fromRawTexts = concat
                . zipWith
                    (\y -> zipWith (\x e -> (e, (x, y))) [0..])
                    [0..]

{-
getContinuousArea :: [(Char, (Int, Int))] -> [(Char, (Int, Int))] -> Int -> DWriter (Char, Area)
getContinuousArea poses pts recurIdx = let nextPoses :: [(Char, (Int, Int))]
                                           nextPoses = filter (\(_, p') -> p' `notElem` map snd pts) poses
                                           neighbs :: [(Char, (Int, Int))]
                                           neighbs = filter (\(c, p) -> any (\(c', p') -> c == c' && tilesAreNextEachOther p p') pts) nextPoses
                                       in do
                                           debug nextPoses
                                           tell "\n"
                                           debug neighbs
                                           tell "\n------------\n"
                                           case neighbs of
                                                [] -> return (fst (head pts), Area (map snd pts))
                                                _  -> do
                                                        contArea <- getContinuousArea nextPoses neighbs (recurIdx + 1)
                                                        return (fst (head pts), Area (map snd pts ++ runTiles (snd contArea)))
                                                        -}

getContinuousArea :: [(Char, (Int, Int))] -> [(Char, (Int, Int))] -> Int -> (Char, Area)
getContinuousArea poses pts recurIdx = let nextPoses :: [(Char, (Int, Int))]
                                           nextPoses = filter (\(_, p') -> p' `notElem` map snd pts) poses
                                           neighbs :: [(Char, (Int, Int))]
                                           neighbs = filter (\(c, p) -> any (\(c', p') -> c == c' && tilesAreNextEachOther p p') pts) nextPoses
                                       in do
                                           case neighbs of
                                                [] -> (fst (head pts), Area (map snd pts))
                                                _  -> let contArea = getContinuousArea nextPoses neighbs (recurIdx + 1)
                                                      in (fst (head pts), Area (map snd pts ++ runTiles (snd contArea)))

fromRawAreas :: [(Char, (Int, Int))] -> [(Char, Area)]
fromRawAreas pts = fst (foldl
                    ( \(zs, zpts) (c, pt) -> if pt `elem` map snd zpts
                                                then let contArea :: (Char, Area)
                                                         contArea = getContinuousArea zpts [(c, pt)] 0
                                                     in (zs ++ [contArea], filter ((`notElem` runTiles (snd contArea)).snd) zpts)
                                                else (zs, zpts)
                    )
                    ([], pts)
                    pts)

raw1 :: [String]
raw1 = [ "OXOOO"
       , "OOOXO"
       ]

raw2 :: [String]
raw2 = [ "CCAB"
       , "CAAA"
       , "CABB"
       ]

uniformizeArea :: Area -> Area
uniformizeArea (Area ts) = Area (DL.sortOn (\(x, y) -> 10000 * y + x) ts)

uniformizeAreas :: [Area] -> [Area]
uniformizeAreas = DL.sortOn (minimum . map (\(x, y) -> 10000 * y + x) . runTiles) . map uniformizeArea

uniformizeAlphabetic :: [(Char, Area)] -> [(Char, Area)]
uniformizeAlphabetic = DL.sortOn ((\(c, ts) -> (minimum . map (\(x, y) -> 10000 * y + x)) ts) . BF.second runTiles) . map (BF.second uniformizeArea)


fromRawAreasComps :: [Comparation [(Char, Area)]]
fromRawAreasComps =
    [ Comparation (701, [('O', Area [(0, 0), (2, 0), (3, 0), (4, 0), (0, 1), (1, 1), (2, 1), (4, 1)])
        , ('X', Area [(1, 0)]), ('X', Area [(3, 1)])], (uniformizeAlphabetic . fromRawAreas . fromRawTexts) raw1)
    , Comparation (702, [('C', Area [(0, 0), (1, 0), (0, 1), (0, 2)]), ('A', Area [(2, 0), (1, 1), (2, 1), (3, 1), (1, 2)])
        , ('B', Area [(3, 0)]), ('B', Area [(2, 2), (3, 2)])], (uniformizeAlphabetic . fromRawAreas . fromRawTexts) raw2)
    ]

completeSum :: [String] -> Integer
completeSum = sum . map (toInteger . cost . snd) . fromRawAreas . fromRawTexts

completeSumComps :: [Comparation Integer]
completeSumComps =
    [ Comparation (801, 152, completeSum raw1)
    , Comparation (802, 116, completeSum raw2)
    ]


testAll :: IO ()
testAll = do
    (putStrLn . unlines . map show) costComps
    {-
    print (getWritten (fromRawAreas (fromRawTexts raw1)))
    print (getWritten (fromRawAreas (fromRawTexts raw2)))
    -}

    (putStrLn . unlines . map show) fromRawAreasComps
    (putStrLn . unlines . map show) completeSumComps

    {-
answer :: IO ()
answer = do
    mass <- getFileLines "day-12.txt"
    (print . completeSum . filter ((>=1).length)) mass
    -}

data Boundary = Up | Right_ | Down | Left_
    deriving Eq

instance Show Boundary where
    show Up     = "¨"
    show Right_ = "]"
    show Down = "_"
    show Left_ = "["

sides :: Area -> Int
sides (Area ts) = let boundrs :: [(Boundary, (Int, Int))]
                      boundrs = concatMap (\p -> map (\b -> (b, p)) (getBoundaries p)) ts
                  in case boundrs of
                          []       -> 0
                          (bdr:bs) -> findSidesFor bs bdr (fst bdr)
    where getBoundaries :: (Int, Int) -> [Boundary]
          getBoundaries (x, y) = (map (\(_, _, b) -> b) . filter (\(px, py, _) -> (px, py) `notElem` ts))
                                    [(x, y - 1, Up), (x + 1, y, Right_), (x, y + 1, Down), (x - 1, y, Left_)]
          -- Walk around the fence, clockwise
          findSidesFor :: [(Boundary, (Int, Int))] -> (Boundary, (Int, Int)) -> Boundary -> Int
          findSidesFor [] (bdir, _)    headB = if bdir == headB then 0 else 1
          findSidesFor bs (Up, (x, y)) headB
              | (Right_, (x, y))        `elem` bs = 1 + findSidesFor (filter (/= (Up, (x, y))) bs) (Right_, (x, y)) headB
              | (Up, (x + 1, y))        `elem` bs =     findSidesFor (filter (/= (Up, (x, y))) bs) (Up, (x + 1, y)) headB
              | (Left_, (x + 1, y - 1)) `elem` bs = 1 + findSidesFor (filter (/= (Up, (x, y))) bs) (Left_, (x + 1, y - 1)) headB
              | otherwise = (if headB == Up then 0 else 1)
                                + findSidesFor (tail bs) (head bs) (fst (head bs))
          findSidesFor bs (Right_, (x, y)) headB
              | (Down, (x, y))       `elem` bs = 1 + findSidesFor (filter (/= (Right_, (x, y))) bs) (Down, (x, y)) headB
              | (Right_, (x, y + 1)) `elem` bs =     findSidesFor (filter (/= (Right_, (x, y))) bs) (Right_, (x, y + 1)) headB
              | (Up, (x + 1, y + 1)) `elem` bs = 1 + findSidesFor (filter (/= (Right_, (x, y))) bs) (Up, (x + 1, y + 1)) headB
              | otherwise = (if headB == Right_ then 0 else 1)
                                + findSidesFor (tail bs) (head bs) (fst (head bs))
          findSidesFor bs (Down, (x, y)) headB
              | (Left_, (x, y))          `elem` bs = 1 + findSidesFor (filter (/= (Down, (x, y))) bs) (Left_, (x, y)) headB
              | (Down, (x - 1, y))       `elem` bs =     findSidesFor (filter (/= (Down, (x, y))) bs) (Down, (x - 1, y)) headB
              | (Right_, (x - 1, y + 1)) `elem` bs = 1 + findSidesFor (filter (/= (Down, (x, y))) bs) (Right_, (x - 1, y + 1)) headB
              | otherwise = (if headB == Down then 0 else 1)
                                + findSidesFor (tail bs) (head bs) (fst (head bs))
          findSidesFor bs (Left_, (x, y)) headB
              | (Up, (x, y))           `elem` bs = 1 + findSidesFor (filter (/= (Left_, (x, y))) bs) (Up, (x, y)) headB
              | (Left_, (x, y - 1))    `elem` bs =     findSidesFor (filter (/= (Left_, (x, y))) bs) (Left_, (x, y - 1)) headB
              | (Down, (x - 1, y - 1)) `elem` bs = 1 + findSidesFor (filter (/= (Left_, (x, y))) bs) (Down, (x - 1, y - 1)) headB
              | otherwise = (if headB == Left_ then 0 else 1)
                                + findSidesFor (tail bs) (head bs) (fst (head bs))

raw3 :: [String]
raw3 =
    [ "AAAAAA"
    , "AAABBA"
    , "AAABBA"
    , "ABBAAA"
    , "ABBAAA"
    , "AAAAAA"
    ]

raw4 :: [String]
raw4 =
    [ "RRRRIICCFF"
    , "RRRRIICCCF"
    , "VVRRRCCFFF"
    , "VVRCCCJFFF"
    , "VVVVCJJCFE"
    , "VVIVCCJJEE"
    , "VVIIICJJEE"
    , "MIIIIIJJEE"
    , "MIIISIJEEE"
    , "MMMISSJEEE"
    ]

sidesComps :: [Comparation Int]
sidesComps =
    [ Comparation (1001, 12, sides (Area [(0, 0), (2, 0), (3, 0), (4, 0), (0, 1), (1, 1), (2, 1), (4, 1)]))
    , Comparation (1002, 6, sides (Area [(0, 0), (1, 0), (0, 1), (0, 2)]))
    , Comparation (1003, 12, sides (Area [(0, 0), (1, 0), (2, 0), (3, 0), (0, 1), (1, 1), (3, 1), (0, 2), (2, 2), (3, 2), (0, 3), (1, 3), (2, 3), (3, 3)]))
    ]

manySidesComps :: [Comparation [(Char, Int)]]
manySidesComps =
    [ Comparation (1201, [('A', 12)], (map (\(c, a) -> (c, sides a) ) . filter ((=='A').fst) .  fromRawAreas . fromRawTexts) raw3)
    , Comparation (1202, [('C', 22), ('C', 4)], (map (\(c, a) -> (c, sides a) ) . filter ((=='C').fst) .  fromRawAreas . fromRawTexts) raw4)
    , Comparation (1203, [('J', 12)], (map (\(c, a) -> (c, sides a) ) . filter ((=='J').fst) .  fromRawAreas . fromRawTexts) raw4)
    ]

discountCost :: Area -> Int
discountCost (Area ts) = sides (Area ts) * length ts

completeDiscountSum :: [String] -> Integer
completeDiscountSum = sum . map (toInteger . discountCost . snd) . fromRawAreas . fromRawTexts

discountComps :: [Comparation Integer]
discountComps =
    [ Comparation (1501, 368, completeDiscountSum raw3)
    , Comparation (1502, 1206, completeDiscountSum raw4)
    ]

testAll2 :: IO ()
testAll2 = do
    (putStrLn . unlines . map show) sidesComps
    (putStrLn . unlines . map show) manySidesComps
    (putStrLn . unlines . map show) discountComps

    --putStrLn (((\(_, a) -> getWritten (sides a)) . head . filter ((=='J').fst) . fromRawAreas . fromRawTexts) raw4)

    {-
answer2 :: IO ()
answer2 = do
    mass <- getFileLines "day-12.txt"
    (print . completeDiscountSum . filter ((>=1).length)) mass
    -}
