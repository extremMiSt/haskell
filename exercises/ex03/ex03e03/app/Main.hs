module Main where
import BoolTerm (prop_pos_empty, prop_pos_valid)
import Test.QuickCheck (quickCheck)

main :: IO ()
main = do
    quickCheck prop_pos_empty
    quickCheck prop_pos_valid