module Main where
import Data.List
import System.Environment

contains :: String -> String -> Bool
string `contains` search = search `isInfixOf` string

grepString :: Bool -> String -> String -> String
grepString b s = unlines . filter ((/= b) . (`contains` s)) . lines  -- (/=) works as XOR on bools

main :: IO ()
main = do
    b <- getArgs
    case length b of --i hate cmd parsing...
        0 -> help
        2 -> action False (b!!0) (b!!1)
        3 -> case head b of
            "-v" -> action True (b!!1) (b!!2)
            x -> help
        x -> help

help :: IO()
help = putStrLn "usage: ... [-v] pattern file"

action :: Bool -> String -> String -> IO ()
action invert pattern file = do
    f <- readFile file
    putStrLn (grepString invert pattern f)
