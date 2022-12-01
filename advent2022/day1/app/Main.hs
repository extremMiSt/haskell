module Main where
import Data.List (sort)

splitParagraphs :: String -> [String]
splitParagraphs [] = []
splitParagraphs [x] = [[x]]
splitParagraphs (x:y:ys) | x /= '\n'  || y /= '\n' = (x:h) :t
                      | x == '\n'  || y == '\n' =  [] : splitParagraphs ys
                      where
                        t = tail (splitParagraphs (y:ys))
                        h = head (splitParagraphs (y:ys))

task1 :: String -> Integer
task1 s = maximum (map (readMaxAll.lines) (splitParagraphs s))


task2 :: String -> Integer
task2 s = sum (take 3 ((reverse.sort) (map (readMaxAll.lines) (splitParagraphs s))))

readMaxAll :: [String] -> Integer
readMaxAll s= sum (map read s)

main :: IO ()
main = do
    f <- readFile "./input.txt"
    print (task1 f)
    print (task2 f)
