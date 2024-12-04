module TestLib (Comparation(Comparation), example) where

newtype Comparation a = Comparation (Int, a, a)

instance (Eq a, Show a) => Show (Comparation a) where
    show (Comparation (line, expected, actual)) = show line
        ++ "- "
        ++ (if expected == actual
            then "🟢"
            else "🔴\n    Expected: " ++ show expected ++ "\n    Actual: " ++ show actual
        )

myAbsSubtraction :: Int -> Int -> Int
myAbsSubtraction x y = abs (x - y)

myFailedAdd :: Int -> Int -> Int
myFailedAdd x _ = x

type IntComparation = Comparation Int
comps :: [IntComparation]
comps =
    [ Comparation (1, 4, (+) 2 2)
    , Comparation (2, 2, myAbsSubtraction 3 1)
    , Comparation (3, 2, myAbsSubtraction 1 3)
    , Comparation (4, 3, myFailedAdd 2 1)
    , Comparation (5, 2, myFailedAdd 2 0)
    ]

example :: IO ()
example = (putStrLn . unlines . map show) comps
