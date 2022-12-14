module Main where
import Data.List

splitGroups :: String -> [String]
splitGroups [] = []
splitGroups [x] = [[x]]
splitGroups (x:y:ys) | x /= '\n'  || y /= '\n' = (x:h) :t
                      | x == '\n'  || y == '\n' =  [] : splitGroups ys
                      where
                        t = tail (splitGroups (y:ys))
                        h = head (splitGroups (y:ys))

someYes :: String -> String
someYes gs = nub (concat (lines gs))

allYes :: String -> String
allYes gs = filter (yes gs) ['a'..'z'] 

yes :: String -> Char -> Bool
yes gs a = foldr ((&&).(a `elem`)) True (lines gs)

main :: IO ()
main = do
    f <- readFile "./input.txt"
    --f <- readFile "./test.txt"
    let gs = splitGroups f
    let cs = map (length.someYes) gs
    print (sum cs)
    let c2s = map (length.allYes) gs
    print (sum c2s)
    return ()