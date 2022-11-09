module Main where
import BoolTerm

main :: IO ()
main = do
    a <- BoolTerm.checkAll
    print a