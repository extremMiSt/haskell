module Main where
import Trie

main :: IO ()
main = do
    b <- Trie.checkAll
    print b
