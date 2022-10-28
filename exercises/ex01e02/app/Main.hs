module Main where

import Test.QuickCheck
import Prelude hiding (subtract)

--data Stack = Stack [Integer]
--instance Eq Stack where 
--   dropwhile (0 ==) (reverse as) == drophile (0 ==) (reverse bs)

push :: a -> [a] -> [a]
--you could "optimize" for pushing 0s in an "empty" stack, but I can't be bothered
push n l = n:l 

prop_push :: Integer -> [Integer] -> Bool
prop_push x y = (head (push x y) == x) && (length y <= length (push x y))

pop :: [a] -> [a]
pop [] = [] --popped the "top" implied 0
pop (_:xs) = xs

prop_pop :: [Integer] -> Bool
prop_pop y = length y >= length (pop y)

dup :: Num a => [a] -> [a]
dup [] = [0,0] --doubled the "top" implied 0
dup (x:xs)  = x:x:xs

prop_dup :: [Integer] -> Bool
prop_dup [] = head (dup []) == (0::Integer) && head (tail (dup [])) == (0::Integer)
prop_dup (x:xs) = head (dup (x:xs)) == head (x:xs) && head (tail (dup (x:xs))) == head (x:xs)

add :: Num a => [a] -> [a]
add [] = [] --added the "top" two 0s
add [x] = [x] --added an implied 0 to the "top" number
add (x:y:xs) = (x+y):xs

prop_add :: [Integer] -> Bool
prop_add [] = null (add []::[Integer])
prop_add [x] = add [x] == [x]
prop_add (x:y:xs) = add (x:y:xs) == (x+y):xs

subtract :: Num a => [a] -> [a]
subtract [] = [] --subtracted the "top" two 0s
subtract [x] = [x] --subtract an implied 0 from the "top" number
subtract (x:y:xs) = (y-x):xs --order makes a diff here, but this feels right

prop_subtract :: [Integer] -> Bool
prop_subtract [] = null (subtract (add []::[Integer])) 
prop_subtract [x] = subtract [x] == [x]
prop_subtract (x:y:xs) = subtract (x:y:xs) == (y-x):xs

multiply :: Num a => [a] -> [a]
multiply [] = [] --multiplied the "top" two 0s
multiply [_] = [] --multiply an implied 0 from the "top" number
multiply (x:y:xs) = (x*y):xs

prop_multiply :: [Integer] -> Bool
prop_multiply [] = null (multiply []::[Integer])
prop_multiply [x] = null (multiply [x]) 
prop_multiply (x:y:xs) = multiply (x:y:xs) == (x*y):xs

neg :: Num a => [a] -> [a]
neg [] = [] --subtracted the "top" two 0s
neg (x:xs) = (-x):xs

prop_neg :: [Integer] -> Bool
prop_neg [] = null (neg ([]::[Integer]))
prop_neg (x:xs) = neg (x:xs) == (-x):xs

prop_task :: Bool
prop_task = head (add (push 8 (pop []))) == (8::Integer)

readCommand :: String -> [Integer] -> [Integer]
readCommand ('p':'u':'s':'h':' ':xs) stack = push (read xs) stack
readCommand "pop" stack = pop stack
readCommand "dup" stack = dup stack
readCommand "add" stack = add stack
readCommand "subtract" stack = subtract stack
readCommand "multiply" stack = multiply stack
readCommand "neg" stack = neg stack
readCommand _ stack = stack


mainLoop :: [Integer] -> IO ()
mainLoop x = do 
    line <- getLine;
    let newStack = readCommand line x
    print newStack
    mainLoop newStack

main :: IO ()
main = do
    print "the example in the task"
    quickCheck prop_task
    print "prop_push"
    quickCheck prop_push
    print "prop_pop"
    quickCheck prop_pop
    print "prop_dup"
    quickCheck prop_dup
    print "prop_add"
    quickCheck prop_add
    print "prop_subtract"
    quickCheck prop_subtract
    print "prop_multiply"
    quickCheck prop_multiply
    print "prop_neg"
    quickCheck prop_neg

    mainLoop [] --start with an empty stack