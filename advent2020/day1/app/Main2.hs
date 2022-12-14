{-# LANGUAGE TupleSections #-}
module Main where
import Data.Maybe (catMaybes, mapMaybe)

pair :: a -> [b] -> [(a,b)] 
pair a = map (a,)

makePairs :: [b] -> [(b, b)]
makePairs [] = []
makePairs (x:xs) = pair x xs ++ makePairs xs

triple :: a -> [(b,c)] -> [(a,b,c)]
triple a = map (\(b,c) -> (a,b,c))

makeTriples :: [c] -> [(c, c, c)]
makeTriples [] = []
makeTriples (x:xs) = triple x (makePairs xs) ++ makeTriples xs

findSum :: (Eq a, Num a) => a -> [(a, a, a)] -> [(a, a,a)]
findSum s = mapMaybe (\t@(a,b,c) -> if (a+b+c)==s then Just t else Nothing)

main :: IO ()
main = do
    f <- readFile "./input.txt"
    let l = map read (lines f)
    let p = makeTriples l
    let (a,b,c) = head (findSum (2020::Integer) p)
    print (a*b*c)

