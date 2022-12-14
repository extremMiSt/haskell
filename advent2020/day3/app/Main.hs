module Main where

trees :: [Int] -> [String] -> String
trees _ [] = []
trees (x:xs) s@(l:ls) = l!!(x `mod` length l) : trees xs ls

trees2 :: [Int] -> [String] -> String
trees2 _ [] = []
trees2 (x:xs) [l] = [l!!(x `mod` length l)]
trees2 (x:xs) s@(l:_:ls) = l!!(x `mod` length l) : trees2 xs ls

threes :: [Int]
threes = map (3*) [0..]

fives :: [Int]
fives = map (5*) [0..]

sevens :: [Int]
sevens = map (7*) [0..]

main :: IO ()
main = do
    f <- readFile "./input.txt"
    let l = lines f
    let r3d1 = length (filter (=='#') (trees threes l))
    putStrLn ("r3d1 and solution part 1: " ++ show r3d1)

    let r1d1 = length (filter (=='#') (trees [0..] l))
    putStrLn ("r1d1: " ++ show r1d1)

    let r5d1 = length (filter (=='#') (trees fives l))
    putStrLn ("r5d1: " ++ show r5d1)

    let r7d1 = length (filter (=='#') (trees sevens l))
    putStrLn ("r7d1: " ++ show r7d1)

    let r1d2  = length (filter (=='#') (trees2 [0..] l))
    putStrLn ("r1d2: " ++ show r1d2)
    print (r3d1*r1d1*r5d1*r7d1*r1d2)
