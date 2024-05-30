{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE QualifiedDo #-}
module Main where
import SimplePrelude as S
import Prelude ()
import Test.QuickCheck (quickCheck)
import Data.List (transpose)

data Queen = Queen | NoQueen
    deriving (Eq, Show)

instance Monad [] where
    return :: a -> [a]
    return x = [x]
    (>>=) :: [a] -> (a -> [b]) -> [b]
    m >>= f = concatMap f m

guard :: Bool -> [()]
guard True = [()]
guard False = []

prop_guardTrue, prop_guardFalse :: [Integer] -> [Integer] -> Bool
prop_guardTrue xs ys = (xs >> guard True >> ys) == (xs >> ys)
prop_guardFalse xs ys = (xs >> guard False >> ys) == []

isSafe :: [[Queen]] -> Int -> Int -> Int -> Bool
isSafe board n xPos yPos = row && column && diag1Down xPos yPos && diag1Up xPos yPos && diag2Down xPos yPos && diag2Up xPos yPos
    where 
        row = (board !! xPos) == replicate n NoQueen
        column = (transpose board !! yPos) == replicate n NoQueen
        diag1Down xPos yPos = (xPos < 0 || yPos < 0) || board !! xPos !! yPos == NoQueen && diag1Down (xPos-1) (yPos-1) 
        diag1Up xPos yPos = (xPos >= n || yPos >= n) || board !! xPos !! yPos == NoQueen && diag1Down (xPos+1) (yPos+1) 
        diag2Down xPos yPos = (xPos < 0 || yPos < 0) || board !! xPos !! yPos == NoQueen && diag1Down (xPos-1) (yPos+1) 
        diag2Up xPos yPos = (xPos >= n || yPos >= n) || board !! xPos !! yPos == NoQueen && diag1Down (xPos+1) (yPos-1) 


search board n placed = S.do
    if n == placed 
        then return board 
        else S.do
            undefined
    
--yeah I don't get this
    

main :: IO ()
main = S.do
    putStrLn "Hello, Haskell!"
    let emptyBoard n = replicate n (replicate n NoQueen)

    quickCheck prop_guardFalse
    quickCheck prop_guardTrue


