module Main where
import Calc

mainLoop :: [Integer] -> IO ()
mainLoop x = do 
    line <- getLine;
    case line of
        "exit" -> return ()
        a -> do
            let newStack = readCommand a x
            print newStack
            mainLoop newStack

main :: IO ()
main = do
    mainLoop [] --start with an empty stack
