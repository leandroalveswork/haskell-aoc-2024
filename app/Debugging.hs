module Debugging (DWriter(DWriter), getWritten, tell, debug) where

newtype DWriter a = DWriter (a, String)
    deriving Eq

getWritten :: DWriter a -> String
getWritten (DWriter (_, text)) = text

instance Functor DWriter where
    fmap f (DWriter (a, t)) = DWriter (f a, t)
instance Applicative DWriter where
    pure x = DWriter (x, "")
    (DWriter (f, t)) <*> (DWriter (a, t')) = DWriter (f a, t ++ t')
instance Monad DWriter where
    return = pure
    (DWriter (a, t)) >>= f = let (DWriter (b, t')) = f a
                            in DWriter (b, t ++ t')

tell :: String -> DWriter ()
tell s = DWriter ((), s)

debug :: (Show a) => a -> DWriter ()
debug = tell . show
