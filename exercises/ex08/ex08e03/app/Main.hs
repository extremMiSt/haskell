module Main where
import Control.Monad.Trans.State.Strict (State, get, put, runState, execState, evalState)

type Random a = State Integer a
fresh :: Random Integer
fresh = do 
    n <- get
    put (next n)
    return (next n)

next :: Integral a => a -> a
next i = (6364136223846793005 * i + 1442695040888963407) `mod` 2^64

runPRNG :: State Integer a -> Integer -> a
runPRNG = evalState


main :: IO ()
main = do 
    let i = runPRNG fresh 1
    print i