{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
module Main where

{-
import qualified System.IO as SIO
import Control.Monad (liftM, ap)

stateIo :: (Read b, Show (IO c)) => FilePath -> (b -> IO c)  -> IO c
stateIo file fun = do 
    f <- SIO.readFile file
    let s = read f
    let s' = fun s
    SIO.writeFile file (show s')
    s'

data StateIO s a = StateIO FilePath (IO (a,s))

runStateIO :: StateIO s a -> IO (a, s)
runStateIO (StateIO path io) = io

instance (Read s, Show s) => Functor (StateIO s) where 
    fmap :: (a -> b) -> StateIO s a -> StateIO s b
    fmap = liftM

instance (Read s, Show s) => Applicative (StateIO s) where
    pure :: a -> StateIO s a
    pure a = StateIO "state.txt" (do
        f <- SIO.readFile "state.txt"
        let s = read f
        return (a, s)
        ) 
    (<*>) :: StateIO s (a -> b) -> StateIO s a -> StateIO s b
    (<*>) = ap

instance (Read s, Show s) => Monad (StateIO s) where 
    (>>=) :: StateIO s a -> (a -> StateIO s b) -> StateIO s b
    st@(StateIO file io1) >>= f = StateIO file (do
        (a1,s1) <- runStateIO st
        (a2,s2) <- (runStateIO.f) a1
        return (a2,s2)
        )

initInt :: StateIO Int Int
initInt = StateIO "state.txt" (do
    SIO.writeFile "state.txt" (show 0)
    return (0,0)
    )
{-
add :: (Num s, Num p, Show s, Read s) => p -> StateIO s b
add n = StateIO "state.txt" (do
    s <- SIO.readFile "state.txt"
    let v = read s
    SIO.writeFile "state.txt" (show (v+n))
    return (0,0)
    )
-}
-}


main :: IO ()
main = putStrLn "Hello, Haskell!"
