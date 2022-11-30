module Main where

isValid :: String -> Bool
isValid s = l <= co && co<=h
    where
        (ls, r1) = split '-' s
        (hs, r2) = split ' ' r1
        (cs, r3) = split ':' r2
        l = read ls
        h = read hs
        c = head cs
        p = drop 1 r3
        co = count c p

isValid2 :: String -> Bool
isValid2 s = (p!!(l-1) == c) /=  (p!!(h-1) == c)
    where
        (ls, r1) = split '-' s
        (hs, r2) = split ' ' r1
        (cs, r3) = split ':' r2
        l = read ls
        h = read hs
        c = head cs
        p = drop 1 r3

split :: (Eq a) => a -> [a] -> ([a],[a])
split a (x:xs) = if x == a 
    then ([], xs) 
    else case split a xs of
        (a,b) -> (x:a, b)

count :: Char -> String -> Integer
count c [] = 0
count c (x:xs) = if c==x 
    then 1 + count c xs
    else count c xs 

main :: IO ()
main = do
    f <- readFile "./input.txt"
    let l = lines f
    print (length (filter isValid l))
    print (length (filter isValid2 l))
