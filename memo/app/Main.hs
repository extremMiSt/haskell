module Main where
import Data.MemoTrie (memoFix)
import Data.Function.Memoize (memoFix)
import System.Clock (Clock(..), getTime)
import Control.Exception (evaluate)
import Data.Function (fix)

main :: IO ()
main = do
    start <- getTime Realtime
    evaluate (fib 40)
    end <- getTime Realtime
    print (end-start);

    start <- getTime Realtime
    evaluate (fixfib 40)
    end <- getTime Realtime
    print (end-start);

fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib x = fib (x-1) + fib (x-2)

fib2 :: (Integer -> Integer) -> Integer -> Integer
fib2 re 0 = 1
fib2 re 1 = 1
fib2 re x = re (x-1) + re (x-2)

fixfib :: Integer -> Integer
fixfib = fix fib2

fibs :: [Integer]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

fib3 :: Int -> Integer
fib3 i = fibs!!i
  where
    fibs = map calculateFib [0..]
    calculateFib 0 = 1
    calculateFib 1 = 1
    calculateFib n = (fibs!!(n-1)) + (fibs!!(n-2))

memoizefib :: Integer -> Integer
memoizefib  = Data.Function.Memoize.memoFix fib2

memotriefib :: Integer -> Integer
memotriefib  = Data.MemoTrie.memoFix fib2