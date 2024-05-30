module Main where

main :: IO ()
main = print (test [1,2,3] ['a','b','c'])

test :: [x] -> [y] -> [(x,y)]
test a b = do
    x <- a
    y <- b
    return (x,y)

