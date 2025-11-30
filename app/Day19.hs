{-# LANGUAGE FlexibleInstances #-}

-- module Day19 (testAll, answer, testAll2, answer2) where
module Day19 (testAll, testAll2) where

import qualified Data.Maybe as DM
import qualified Data.List as DL
import qualified Data.List.Split as DLS

import TestLibrary (runEverything, Assertable (toAssertion), toBasicAssertion, toEnumAssertion, Label (label), assert, markProperty)
import DataMassParsers (getFileLines)

data Stripe = StW | StU | StB | StR | StG
  deriving (Eq, Show)

newtype Towel = Towel [Stripe]
  deriving (Eq, Show)

newtype TowelMap = TowelMap ([Towel], [Towel], [Towel], [Towel], [Towel])
lookupTowel :: Stripe -> TowelMap -> [Towel]
lookupTowel StW (TowelMap (y, _, _, _, _)) = y
lookupTowel StU (TowelMap (_, y, _, _, _)) = y
lookupTowel StB (TowelMap (_, _, y, _, _)) = y
lookupTowel StR (TowelMap (_, _, _, y, _)) = y
lookupTowel StG (TowelMap (_, _, _, _, y)) = y

permutations :: [Stripe] -> TowelMap -> [[Towel]]
permutations [] _ = []
permutations stripes@(stripe:_) xs =
  let towels :: [Towel]
      towels = lookupTowel stripe xs
      fitTowels :: [Towel]
      fitTowels = filter (\(Towel t) -> t == take (length t) stripes) towels
  in concatMap
      (\(Towel t) ->
        let cutStripes = drop (length t) stripes
        in case cutStripes of
            [] -> [[Towel t]]
            stripes' -> map (Towel t:) $ permutations stripes' xs)
      fitTowels

hasPermutationInAnyDesign :: [[Stripe]] -> TowelMap -> Bool
hasPermutationInAnyDesign [] _ = False
hasPermutationInAnyDesign designs xs =
  let linesLeft :: [[Stripe]]
      linesLeft =
        DL.nub
        $ concat
        $ DM.mapMaybe
            (\stripes ->
                case stripes of
                      [] -> Nothing
                      (stripe:_) ->
                        let towels :: [Towel]
                            towels = lookupTowel stripe xs
                            fitTowels :: [Towel]
                            fitTowels = filter (\(Towel t) -> t == take (length t) stripes) towels
                        in Just
                            $ map (\(Towel t) -> drop (length t) stripes) fitTowels
            )
            designs
  in if any null linesLeft
        then True
        else hasPermutationInAnyDesign linesLeft xs

hasPermutation :: [Stripe] -> TowelMap -> Bool
hasPermutation stripes = hasPermutationInAnyDesign [stripes]

createDict :: [Towel] -> TowelMap
createDict xs =
  TowelMap
    ( filter (\(Towel (x:_)) -> x == StW) xs
    , filter (\(Towel (x:_)) -> x == StU) xs
    , filter (\(Towel (x:_)) -> x == StB) xs
    , filter (\(Towel (x:_)) -> x == StR) xs
    , filter (\(Towel (x:_)) -> x == StG) xs
    )

stripeFromChar :: Char -> Maybe Stripe
stripeFromChar 'w' = Just StW
stripeFromChar 'u' = Just StU
stripeFromChar 'b' = Just StB
stripeFromChar 'r' = Just StR
stripeFromChar 'g' = Just StG
stripeFromChar _ = Nothing

stripeListFromString :: String -> Maybe [Stripe]
stripeListFromString = foldr (\c m -> m >>= \m' -> (stripeFromChar c) >>= \c' -> Just $ c':m') (Just [])

towelsFromString :: String -> Maybe TowelMap
towelsFromString =
  fmap createDict
    . foldr (\line m -> m >>= \m' -> (stripeListFromString line) >>= \line' -> Just $ (Towel line'):m') (Just [])
    . DLS.splitOn ", "

instance Assertable Stripe where
  toAssertion = toBasicAssertion

instance Assertable Towel where
  toAssertion (Towel a) (Towel b) = toEnumAssertion a b

instance Assertable x => Assertable [x] where
  toAssertion = toEnumAssertion

instance Assertable x => Assertable (Maybe x) where
  toAssertion a b
    | DM.isJust a /= DM.isJust b = toAssertion a b
    | DM.isNothing a = []
    | otherwise = toAssertion (DM.fromJust a) (DM.fromJust b)

instance Assertable TowelMap where
  toAssertion (TowelMap (aW, aU, aB, aR, aG)) (TowelMap (bW, bU, bB, bR, bG)) =
    (markProperty "byW" $ toEnumAssertion aW bW)
      ++ (markProperty "byU" $ toEnumAssertion aU bU)
      ++ (markProperty "byB" $ toEnumAssertion aB bB)
      ++ (markProperty "byR" $ toEnumAssertion aR bR)
      ++ (markProperty "byG" $ toEnumAssertion aG bG)

instance Assertable Bool where
  toAssertion = toBasicAssertion

testReaders :: [Maybe String]
testReaders =
  [ assert (Just StW) (stripeFromChar 'w') "101"
  , assert (Just StR) (stripeFromChar 'r') "102"
  , assert (Just StG) (stripeFromChar 'g') "103"
  , assert Nothing (stripeFromChar 'G') "104"
  , assert (Just [StW, StB, StB]) (stripeListFromString "wbb") "105"
  , assert (Just [StR, StG, StB, StG, StW]) (stripeListFromString "rgbgw") "106"
  , assert (Just [StU]) (stripeListFromString "u") "107"
  , assert Nothing (stripeListFromString "web") "108"
  , assert
      (Just $ TowelMap
          ( []
          , [Towel [StU, StU, StB], Towel [StU, StW, StU, StG, StR, StG, StU]]
          , []
          , [Towel [StR, StR, StB, StR, StB, StW]]
          , []
          )
      )
      (towelsFromString "uub, rrbrbw, uwugrgu")
      "109"
  ]

testPermutations :: [Maybe String]
testPermutations =
  [ assert
      ( [ [Towel [StR, StB], Towel [StG], Towel [StG]]
        , [Towel [StR, StB], Towel [StG, StG]]
        ]
      )
      (_permutations "rbgg" "rb, g, gg, bg")
      "201"
  , assert
      ( [ [Towel [StG, StW, StR], Towel [StR, StU]]
        , [Towel [StG, StW], Towel [StR, StR, StU]]
        ]
      )
      (_permutations "gwrru" "gwr, ru, gw, rru, g")
      "202"
  , assert [] (_permutations "gwrru" "gw, g, ru") "203"
  , assert True (_hasPermutation "rbgg" "rb, g, gg, bg") "204"
  , assert True (_hasPermutation "gwrru" "gwr, ru, gw, rru, g") "205"
  , assert False (_hasPermutation "gwrru" "gw, g, ru") "206"
  ]
  where _permutations :: String -> String -> [[Towel]]
        _permutations rawStripes rawTowels =
          let stripes = DM.fromJust $ stripeListFromString rawStripes
              towels = DM.fromJust $ towelsFromString rawTowels
          in permutations stripes towels
        _hasPermutation :: String -> String -> Bool
        _hasPermutation rawStripes rawTowels =
          let stripes = DM.fromJust $ stripeListFromString rawStripes
              towels = DM.fromJust $ towelsFromString rawTowels
          in hasPermutation stripes towels

testAll :: IO ()
testAll = runEverything
  $ testReaders
    ++ testPermutations

{-
answer :: IO ()
answer = do
  mass <- getFileLines "day-19.txt"
  let stripeDesigns :: [[Stripe]]
      stripeDesigns = map (DM.fromJust . stripeListFromString) (drop 2 mass)
      towels :: TowelMap
      towels = DM.fromJust $ towelsFromString (mass !! 0)
    in print $ length $ filter id $ map (\s -> hasPermutation s towels) stripeDesigns
    -}

concatCounts :: Eq a => [[(Integer, a)]] -> [(Integer, a)]
concatCounts =
  foldr
    (\xs acc ->
      foldr
        (\(xQt, x) acc' ->
          case DL.find ((==x).snd) acc' of
                Nothing -> (xQt, x):acc'
                Just (qt, _) -> (qt + xQt, x): filter ((/=x).snd) acc'
        )
        acc
        xs
    )
    []

instance Assertable (Integer, String) where
  toAssertion = toBasicAssertion

testGroupCount :: [Maybe String]
testGroupCount =
  [ assert [(2, "Orb"), (2, "Sword"), (3, "Nord")] (concatCounts [[(1, "Orb"), (2, "Sword")], [(1, "Orb"), (3, "Nord")]]) "1001"
  , assert [] (concatCounts ( []::[[(Integer, String)]] )) "1002"
  , assert [(1, "U"), (1, "")] (concatCounts [[(1, "U")], [(1, "")]]) "1003"
  ]

_permutationsLength :: [(Integer, [Stripe])] -> TowelMap -> [(Integer, [Stripe])]
_permutationsLength [] _ = []
_permutationsLength designs xs =
  let linesLeft :: [(Integer, [Stripe])]
      linesLeft =
        concatCounts
        $ map
            (\(qtt, stripes@(stripe:_)) ->
                let towels :: [Towel]
                    towels = lookupTowel stripe xs
                    fitTowels :: [Towel]
                    fitTowels = filter (\(Towel t) -> t == take (length t) stripes) towels
                in map (\(Towel t) -> (qtt, drop (length t) stripes)) fitTowels
            )
        $ filter
            (not . null . snd)
            designs
      endComponent :: [(Integer, [Stripe])]
      endComponent = filter (null . snd) linesLeft ++ (filter (null . snd) designs)
  in endComponent ++ _permutationsLength (filter (not . null . snd) linesLeft) xs

permutationsLength :: [Stripe] -> TowelMap -> Integer
permutationsLength design = sum . map fst . _permutationsLength [(1, design)]

instance Assertable Integer where
  toAssertion = toBasicAssertion

testPermutationLength :: [Maybe String]
testPermutationLength =
  [ assert 2 (_runPermLength "rbgg" "rb, g, gg, bg") "1101"
  , assert 2 (_runPermLength "gwrru" "gwr, ru, gw, rru, g") "1102"
  , assert 8 (_runPermLength "gwbrgwbrgw" "g, w, br, gw") "1103"
  ]
  where _runPermLength :: String -> String -> Integer
        _runPermLength rawStripes rawTowels =
          let stripes = DM.fromJust $ stripeListFromString rawStripes
              towels = DM.fromJust $ towelsFromString rawTowels
          in permutationsLength stripes towels

testAll2 :: IO ()
testAll2 = runEverything
  $ testGroupCount
    ++ testPermutationLength

{-
answer2 :: IO ()
answer2 = do
  mass <- getFileLines "day-19.txt"
  let stripeDesigns :: [[Stripe]]
      stripeDesigns = map (DM.fromJust . stripeListFromString) (drop 2 mass)
      towels :: TowelMap
      towels = DM.fromJust $ towelsFromString (mass !! 0)
    in print
        $ sum
        $ map (\s -> permutationsLength s towels)
        $ filter (\s -> hasPermutation s towels) stripeDesigns
    -}
