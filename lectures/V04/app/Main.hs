module Main where

curry' :: ((a, b) -> t) -> a -> b -> t
curry' f x y = f (x,y) 

uncurry' :: (t1 -> t2 -> t3) -> (t1, t2) -> t3
uncurry' f (x,y) = f x y

foldr' :: (t1 -> t2 -> t2) -> t2 -> [t1] -> t2
foldr' f z  [] = z
foldr' f z (x:xs) = f x (foldr' f z xs)

or' :: [Bool] -> Bool
or' = foldr' (||) False

and' :: [Bool] -> Bool
and' = foldr' (&&) True

isSpace :: Char -> Bool
isSpace ' ' = True
isSpace '\n' = True
isSpace x = False

a :: a -> [a] -> [a]
a = (\ x -> (++[x]))

main :: IO ()
main = do
    let x = uncurry (+)
    let y = x (1,2)
    print y
