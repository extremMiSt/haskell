{-# LANGUAGE TupleSections #-}
module Main where
import Data.Maybe (catMaybes, mapMaybe)


pair :: a -> [b] -> [(a,b)] 
pair a = map (a,)

makePairs :: [b] -> [(b, b)]
makePairs [] = []
makePairs (x:xs) = pair x xs ++ makePairs xs

findSum :: (Eq a, Num a) => a -> [(a, a)] -> [(a, a)]
findSum s = mapMaybe (\t@(a,b) -> if a+b==s then Just t else Nothing)

main :: IO ()
main = do
    f <- readFile "./input.txt"
    let l = map read (lines f)
    let p = makePairs l
    let (a,b) = head (findSum (2020::Integer) p)
    print (a*b)
    --print (commonPrefix "aaaabb" "aaaabc")

--commonPrefix :: String -> String -> String
--commonPrefix [] _ = []
--commonPrefix _ [] = []
--commonPrefix (x:xs) (y:ys) | x==y = x:commonPrefix xs ys
--commonPrefix (x:xs) (y:ys) | x/=y = []
