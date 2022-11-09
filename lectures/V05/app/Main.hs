module Main where

nat :: [Integer]
nat = [0..]

ones :: [Integer]
ones = [1 | x <- [0..]]

ones' :: [Integer]
ones' = 1 : ones

fib :: [Integer]
fib = 0 : 1 : zipWith (+) fib (tail fib)

badfib :: [Integer]
badfib = 0 : zipWith (+) badfib (tail badfib)

primes :: [Integer]
primes = sieve [2..]
    where 
    sieve (l:ls) = l : sieve (filter (\x -> x `mod` l /= 0) ls)

data BTree = Leaf Integer | Branch BTree BTree
    deriving (Show)

mintree :: BTree -> BTree
mintree b = mb
    where 
    (ib, mb) = helper ib b
    helper :: Integer -> BTree -> (Integer,BTree)
    helper m (Leaf i) = (i, Leaf m) 
    helper m (Branch l r) = 
        let (lm, lb) = helper m l 
            (rm, rb) = helper m r
        in (min lm rm, Branch lb rb)

f :: (Read a1, Show a2) => a2 -> a1
f x = read (show x)

g :: String -> String
g x = show (read x::Integer)

main :: IO ()
main = print (mintree (Branch (Leaf 42) (Leaf 17)))
