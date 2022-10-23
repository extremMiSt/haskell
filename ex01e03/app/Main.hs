module Main where

head :: [a] -> a
head [] = error "no head"
head (x:_) = x

tail :: [a] -> [a]
tail [] = error "no tail"
tail (_:xs) = xs

last :: [a] -> a
last [] = error "no last"
last [x] = x
last (_:xs) = Main.last xs

length :: [a] -> Integer
length [] = 0
length (_:xs) =  1 + Main.length xs

and :: [Bool] -> Bool
and [] = True
and (x:xs) = x && Main.and xs

init :: [a] -> [a]
init [] = error "no last"
init [_] = []
init (x:xs) = x : Main.init xs

(++) :: [a] -> [a] -> [a]
[] ++ x = x
x ++ y = (Main.init x) Main.++ (Main.last x : y)

zip :: [a] -> [b] -> [(a, b)]
zip [] _ = []
zip _ [] = []
zip (a:as) (b:bs)  = (a,b):Main.zip as bs

reverse :: [a] -> [a]
reverse [] = []
reverse x = Main.last x : Main.init x

main :: IO ()
main = putStrLn ("Hello, Haskell!" Main.++ " test")
