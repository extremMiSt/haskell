module Main where

data These a b = This a | That b | These a b

instance (Semigroup a) => Monad (These [Integer] Integer) where
    This  a   >>= _ = This a
    That    x >>= k = k x
    These a x >>= k = case k x of
                          This  b   -> This  (a <> b)
                          That    y -> These a y
                          These b y -> These (a <> b) y

main :: IO ()
main = putStrLn "Hello, Haskell!"
