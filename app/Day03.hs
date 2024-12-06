module Day03 (testAll, testAll2) where
--module Day03 (testAll, answer, testAll2, answer2) where

import qualified Text.Read as TR

import TestLib (Comparation(Comparation))
import DataMassParsers (getFileLines)

data Symbol = Mul | Number Int | SingleChar Char | DontMul | DoMul

readOnly0to9 :: String -> Maybe Int
readOnly0to9 x = (if any (`notElem` ['0'..'9']) x then Nothing else Just x)
    >>= TR.readMaybe

toIntMaxDigits :: Int -> String -> (Maybe Int, Int)
toIntMaxDigits 0 _     = (Nothing, 0)
toIntMaxDigits 1 []    = (Nothing, 0)
toIntMaxDigits 1 (c:_) = case readOnly0to9 [c] of Just a  -> (Just a, 1)
                                                  Nothing -> (Nothing, 0)
toIntMaxDigits n cs
    | length cs < n = toIntMaxDigits (n - 1) cs
    | otherwise = case readOnly0to9 (take n cs) of
                       Just okV -> (Just okV, n)
                       Nothing  -> toIntMaxDigits (n - 1) cs

toSymbols :: String -> [Symbol]
toSymbols "" = []
toSymbols ('m':'u':'l':cs) = Mul : toSymbols cs
toSymbols ('d':'o':'(':')':cs) = DoMul : toSymbols cs
toSymbols ('d':'o':'n':'\'':'t':'(':')':cs) = DontMul : toSymbols cs
toSymbols (c:cs) = let int_ = toIntMaxDigits 3 (c:cs)
                   in case fst int_ of
                        Just v  -> Number v : ( toSymbols . drop (snd int_) ) (c:cs)
                        Nothing -> SingleChar c : toSymbols cs

newtype MulOperation = MulOperation (Int, Int)

retrieveMulOperations :: [Symbol] -> [MulOperation]
retrieveMulOperations [] = []
retrieveMulOperations
    ( Mul
    : SingleChar '('
    : Number a
    : SingleChar ','
    : Number b
    : SingleChar ')'
    : cs
    ) = MulOperation (a, b) : retrieveMulOperations cs
retrieveMulOperations (_:cs) = retrieveMulOperations cs

sumMuls :: [MulOperation] -> Int
sumMuls = sum . map (\(MulOperation (x, y)) -> x * y)

instance Eq Symbol where
    Mul == Mul = True
    Mul == _   = False
    SingleChar c == SingleChar d = c == d
    SingleChar c == _            = False
    Number n == Number o = n == o
    Number n == _        = False
    DontMul == DontMul = True
    DontMul == _       = False
    DoMul == DoMul = True
    DoMul == _     = False

instance Show Symbol where
    show Mul = "✖️"
    show (SingleChar c) = [c]
    show (Number n) = 'n': show n
    show DontMul = "🚫"
    show DoMul = "⬆️"

symbols1 = [ SingleChar 'x', Mul, SingleChar '(', Number 2, SingleChar ',', Number 4, SingleChar ')'
    , SingleChar '%', SingleChar '&', Mul, SingleChar '[', Number 3, SingleChar ',', Number 7, SingleChar ']'
    ] ++ map SingleChar "!@^do_not_" ++
    [ Mul, SingleChar '(', Number 5, SingleChar ',', Number 5, SingleChar ')', SingleChar '+', Mul
    , SingleChar '(', Number 32, SingleChar ',', Number 64, SingleChar ']'
    ] ++ map SingleChar "then(" ++
    [ Mul, SingleChar '(', Number 11, SingleChar ',', Number 8, SingleChar ')', Mul, SingleChar '('
    , Number 8, SingleChar ',', Number 5, SingleChar ')', SingleChar ')'
    ]
symbols2 = [ Mul, SingleChar '(', Number 209, Number 0, SingleChar ',', Number 18, SingleChar ')', Mul, SingleChar '('
    , Number 2, SingleChar ',', SingleChar ' ', Number 1, SingleChar ')', SingleChar '-', Mul, SingleChar '(', Number 3
    , SingleChar '.', Number 1, SingleChar ')', Mul, SingleChar '(', Number 3, SingleChar ',', Number 1, SingleChar ')'
    , SingleChar '.', SingleChar '.'
    ]

symbolComps :: [Comparation [Symbol]]
symbolComps =
    [ Comparation (1, symbols1, toSymbols "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))")
    , Comparation (2, symbols2, toSymbols "mul(20900,18)mul(2, 1)-mul(3.1)mul(3,1)..")
    ]

sumComps :: [Comparation Int]
sumComps =
    [ Comparation (11, 161 , (sumMuls . retrieveMulOperations) symbols1)
    , Comparation (12, 3, (sumMuls . retrieveMulOperations) symbols2)
    ]

testAll :: IO ()
testAll = do
    (putStrLn . unlines . map show) symbolComps
    (putStrLn . unlines . map show) sumComps

    {-
answer :: IO ()
answer = do
    mass <- getFileLines "day-03.txt"
    print $ (sumMuls . retrieveMulOperations . toSymbols) $ unlines mass
    -}

retrieveSmartMuls :: [Symbol] -> [MulOperation]
retrieveSmartMuls s = retrieveSmartMuls' s True

retrieveSmartMuls' :: [Symbol] -> Bool -> [MulOperation]
retrieveSmartMuls' [] _ = []
retrieveSmartMuls'
    ( Mul
    : SingleChar '('
    : Number a
    : SingleChar ','
    : Number b
    : SingleChar ')'
    : cs
    ) enableMul = (if enableMul then (MulOperation (a, b):) else id) (retrieveSmartMuls' cs enableMul)
retrieveSmartMuls' (DoMul:cs) _ = retrieveSmartMuls' cs True
retrieveSmartMuls' (DontMul:cs) _ = retrieveSmartMuls' cs False
retrieveSmartMuls' (_:cs) enableMul = retrieveSmartMuls' cs enableMul

symbols101 = [ SingleChar 'x', Mul, SingleChar '(', Number 2, SingleChar ',', Number 4, SingleChar ')'
    , SingleChar '&', Mul, SingleChar '[', Number 3, SingleChar ',', Number 7, SingleChar ']'
    , SingleChar '!', SingleChar '^', DontMul, SingleChar '_'
    , Mul, SingleChar '(', Number 5, SingleChar ',', Number 5, SingleChar ')', SingleChar '+', Mul
    , SingleChar '(', Number 32, SingleChar ',', Number 64, SingleChar ']', SingleChar '(', Mul
    , SingleChar '(', Number 11, SingleChar ',', Number 8, SingleChar ')', SingleChar 'u', SingleChar 'n'
    , DoMul, SingleChar '?', Mul, SingleChar '(', Number 8, SingleChar ',', Number 5, SingleChar ')', SingleChar ')'
    ]
symbols102 = [ DontMul, SingleChar 'd', SingleChar 'o'
    , SingleChar '(', Number 209, Number 0, SingleChar ',', Number 18, SingleChar ')', Mul, SingleChar '('
    , Number 2, SingleChar ',', Number 1, SingleChar ')'
    ] ++ map SingleChar "-do( )" ++
    [ Mul, SingleChar '(', Number 1, SingleChar ',', Number 1, SingleChar ')', DoMul
    , Mul, SingleChar '(', Number 3, SingleChar ',', Number 1, SingleChar ')'
    ]

smartSymbolComps :: [Comparation [Symbol]]
smartSymbolComps =
    [ Comparation (101, symbols101, toSymbols "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")
    , Comparation (102, symbols102, toSymbols "don't()do(20900,18)mul(2,1)-do( )mul(1,1)do()mul(3,1)")
    ]

smartSumComps :: [Comparation Int]
smartSumComps =
    [ Comparation (111, 48 , (sumMuls . retrieveSmartMuls) symbols101)
    , Comparation (112, 3, (sumMuls . retrieveSmartMuls) symbols102)
    ]

testAll2 :: IO ()
testAll2 = do
    (putStrLn . unlines . map show) smartSymbolComps
    (putStrLn . unlines . map show) smartSumComps

    {-
answer2 :: IO ()
answer2 = do
    mass <- getFileLines "day-03.txt"
    print $ (sumMuls . retrieveSmartMuls . toSymbols) $ unlines mass
    -}
