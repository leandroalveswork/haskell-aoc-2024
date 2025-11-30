module TestLibrary
  ( Assertable, toAssertion, toBasicAssertion, toEnumAssertion, markProperty
  , assert, enumAssert
  , Label, label
  , runUntilFail, runEverything
  , example2
  ) where

import qualified Data.Maybe as DM
import qualified Data.List as DL
import Control.Monad (join)

class Assertable a where
  toAssertion :: a -> a -> [(String, String)]

toBasicAssertion :: (Show e, Eq e) => e -> e -> [(String, String)]
toBasicAssertion expected actual =
  [cd | cd <- [("Expected = " ++ show expected, "Actual   = " ++ show actual)], expected /= actual]

markProperty :: String -> [(String, String)] -> [(String, String)]
markProperty propertyName =
  map
    (\(d, e) -> 
        ( propertyName ++ ": " ++ d
        , (replicate (length propertyName + 2) ' ') ++ e
        )
    )

class Label l where
  label :: l x -> [(String, x)]

instance Label [] where
  label = zipWith (\i e -> (show i, e)) ([0..] :: [Int])

toEnumAssertion :: (Assertable x, Label t) => t x -> t x -> [(String, String)]
toEnumAssertion es xs =
  let eList = label es
      xList = label xs
      missingKeys :: [String]
      missingKeys = filter (`notElem` (map fst xList)) (map fst eList)
      extraKeys :: [String]
      extraKeys = filter (`notElem` (map fst eList)) (map fst xList)
      differentValues :: [(String, String)]
      differentValues =
        concatMap (\(key, eValue, xValue) -> markProperty ("[" ++ key ++ "]") $ toAssertion eValue xValue)
        $ DM.mapMaybe 
            (\(eKey, eValue) -> do
              foundTup <- DL.find (\(xKey, _) -> xKey == eKey) xList
              return (eKey, eValue, snd foundTup)
            ) 
            eList
  in markProperty "missingKeys" (toBasicAssertion [] missingKeys)
      ++ markProperty "extraKeys" (toBasicAssertion [] extraKeys)
      ++ differentValues

_assert :: Assertable a => a -> a -> String
_assert x = DL.intercalate "\n\n" . map (\(line, line') -> line ++ "\n" ++ line') . toAssertion x

assert :: Assertable a => a -> a -> String -> Maybe String
assert x x' tag =
  case _assert x x' of
        []  -> Nothing
        msg -> Just ("Test [" ++ tag ++ "] failed!\n" ++ "\n" ++ msg)

_enumAssert :: (Assertable x, Label t) => t x -> t x -> String
_enumAssert x = DL.intercalate "\n\n" . map (\(line, line') -> line ++ "\n" ++ line') . toEnumAssertion x

enumAssert :: (Assertable x, Label t) => t x -> t x -> String -> Maybe String
enumAssert x x' tag =
  case _enumAssert x x' of
        []  -> Nothing
        msg -> Just ("Test [" ++ tag ++ "] failed!\n" ++ "\n" ++ msg)

runUntilFail :: [Maybe String] -> IO ()
runUntilFail xs =
  let failed :: Maybe String
      failed = join $ DL.find DM.isJust xs
  in case failed of
          Nothing -> putStrLn (show (length xs) ++ " tests passed.")
          Just err -> error err

runEverything :: [Maybe String] -> IO ()
runEverything xs =
  let failed :: [String]
      failed = DM.mapMaybe id xs
  in if null failed
        then putStrLn (show (length xs) ++ " tests passed.")
        else error $ DL.intercalate "\n" failed

data Point = Point { ptX :: Int, ptY :: Int }
  deriving (Eq, Show)

instance Assertable Point where
  toAssertion expected actual =
     (markProperty "ptX" $ toBasicAssertion (ptX expected) (ptX actual))
       ++ (markProperty "ptY" $ toBasicAssertion (ptY expected) (ptY actual))

myInvertAxes :: Point -> Point
myInvertAxes p = Point { ptX = -ptX p, ptY = -ptY p }

failedInvert :: Point -> Point
failedInvert p = p { ptX = -ptX p }

newtype TenPoints = TenPoints [Point]

instance Assertable TenPoints where
  toAssertion (TenPoints expected) (TenPoints actual) =
    toEnumAssertion expected actual

myTakeFirst :: TenPoints -> TenPoints
myTakeFirst = TenPoints . take 1 . (\(TenPoints xs) -> xs)

failedTakeFirst :: TenPoints -> TenPoints
failedTakeFirst = id

failedTakeFirst' :: TenPoints -> TenPoints
failedTakeFirst' = (\(TenPoints xs) -> TenPoints (map myInvertAxes xs)) . myTakeFirst

example2 :: IO ()
example2 = runEverything
  [ assert (mkPoint 1 2) (myInvertAxes $ mkPoint (-1) (-2)) "Invert axes, 1"
  , assert (mkPoint 0 25) (myInvertAxes $ mkPoint 0 (-25)) "Invert axes, 2"
  , assert (mkPoint 1 2) (failedInvert $ mkPoint (-1) (-2)) "Invert axes, 3"
  , assert (newL [mkPoint 1 4]) (myTakeFirst $ newL $ replicate 10 $ mkPoint 1 4) "Take first, 1"
  , assert (newL [mkPoint 1 4]) (failedTakeFirst $ newL $ replicate 10 $ mkPoint 1 4) "Take first, 2"
  , assert (newL [mkPoint 1 4]) (failedTakeFirst' $ newL $ replicate 10 $ mkPoint 1 4) "Take first, 3"
  ]
  where mkPoint :: Int -> Int -> Point
        mkPoint x y = Point { ptX = x, ptY = y }
        newL :: [Point] -> TenPoints
        newL = TenPoints

