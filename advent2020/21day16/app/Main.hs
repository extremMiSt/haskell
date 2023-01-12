module Main where
import Text.Printf

toBin :: IsChar a => [Char] -> [a]
toBin (s:ss) = bin ++ toBin ss
    where 
        bin = printf "%s" (read [s]::Int)

            

main :: IO ()
main = putStrLn "Hello, Haskell!"
