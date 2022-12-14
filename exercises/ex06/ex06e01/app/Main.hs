module Main where

import System.Random
import Text.Read

data Result = Gr | Le | Eq 

getAnswer :: IO Result
getAnswer = do
    s <- getLine
    case s of
        "greater" -> return Gr
        "smaller" -> return Le
        "yes" -> return Eq
        a -> getAnswer

gameLoop ::(Integer, Integer) -> IO ()
gameLoop (mi,ma) = do
    i <- randomRIO (mi,ma)
    putStrLn ("Is it " ++ show i ++ "?")
    s <- getAnswer
    case s of
        Eq -> return ()
        Gr -> gameLoop (i+1, ma)
        Le -> gameLoop (mi, i-1)

main :: IO ()
main = do 
    putStrLn "Choose a number between 1 and 100!"
    gameLoop (1, 100)
