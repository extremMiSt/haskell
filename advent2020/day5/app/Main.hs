module Main where
import Data.List

fromBinary :: (Foldable t, Num b, Eq a) => a -> t a -> b
fromBinary o = foldl (\s n -> s*2 + if n==o then 1 else 0) 0

seatId :: [Char] -> Integer
seatId s = (fromBinary 'B' (take 7 s) * 8) +  fromBinary 'R' (drop 7 s)

findMissing :: [Integer] -> Integer
findMissing (x:y:ys) = if (x+1) /= y
    then x+1
    else findMissing (y:ys)

main :: IO ()
main = do
    f <- readFile "./input.txt"
    --f <- readFile "./test.txt"
    let l = map seatId (lines f)
    print (maximum l)
    let lsorted = sort l
    print (findMissing lsorted)
    return ()
