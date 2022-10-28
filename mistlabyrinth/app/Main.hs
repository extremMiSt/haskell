module Main where
import Data.Type.Equality (apply)
import Data.List (genericTake, genericDrop, genericIndex)
import System.Console.Haskeline(runInputT, defaultSettings, getInputChar)
import Lab

betterInputChar :: IO Char
betterInputChar = do
  mc <- runInputT defaultSettings (getInputChar "")
  case mc of
    Nothing -> betterInputChar
    (Just c) -> return c

playmap :: [[Integer]]
playmap = [
    [0,1,1,1,1,1],
    [0,0,0,0,0,1],
    [0,1,1,1,1,1],
    [1,0,0,0,0,0],
    [0,1,1,1,1,1],
    [0,0,0,0,0,1]
    ]

playerPos :: (Integer, Integer)
playerPos = (1,1);

applyPlayer :: [String] -> (Integer, Integer) -> [String]
applyPlayer l p = setAt2 p l 'X'

getAt :: (Integer, Integer) -> [String] -> Char
getAt (x,y) gamemap = genericIndex (genericIndex gamemap y) x

setAt2 :: (Integer, Integer) -> [[a]] -> a -> [[a]]
setAt2 (x,y) gamemap s = genericTake y gamemap ++ setAt1 x (genericIndex gamemap y) s : genericDrop (y+1) gamemap

setAt1 ::Integer -> [a] -> a -> [a]
setAt1 x l r = genericTake x l ++ r : genericDrop (x+1) l

toGui :: (Eq a1, Eq a2, Num a1, Num a2) => [([a2], a1)] -> [String]
toGui gamemap = topWall (length gamemap):map toLine gamemap

toLine :: (Eq a1, Eq a2, Num a1, Num a2) => ([a2], a1) -> [Char]
toLine (x,0) = foldr (++) "|" (map mapSymbol0 x)
toLine (x,1) = foldr (++) "*" (map mapSymbol1 x)

topWall :: (Eq t, Num t) => t -> [Char]
topWall 0 = "*";
topWall n = "*--" ++ topWall (n-1)

mapSymbol0 :: (Eq a, Num a) => a -> String
mapSymbol0 0 = "|  "
mapSymbol0 1 = "   "

mapSymbol1 :: (Eq a, Num a) => a -> String
mapSymbol1 0 = "*--"
mapSymbol1 1 = "*  "

nextPos :: (Integer, Integer) -> String -> [String] -> (Integer, Integer)
nextPos (x,y) "w" gm | getAt (x,y-1) gm == ' ' = (x,y-1)
nextPos (x,y) "s" gm | getAt (x,y+1) gm == ' ' = (x,y+1)
nextPos (x,y) "a" gm | getAt (x-1,y) gm == ' ' = (x-1,y)
nextPos (x,y) "d" gm | getAt (x+1,y) gm == ' ' = (x+1,y)
nextPos x _ _ = x -- ignore invalid input

gameLoop :: (Eq a2, Num a2) => [[a2]] -> (Integer, Integer) -> IO ()
gameLoop playmap playerPos = do 
    --putStrLn "\ESC[2J"
    putStrLn (unlines (applyPlayer (toGui (zip playmap (cycle [0,1]))) playerPos))
    --s <- getLine
    s <- betterInputChar

    let newPos = nextPos playerPos [s] (toGui (zip playmap (cycle [0,1])))
    let won =  snd playerPos >= toInteger (length playmap)

    if won 
        then return ()
        else gameLoop playmap newPos 

main :: IO ()
main =  do 
    gameLoop playmap playerPos
    putStrLn "You Won! Congrats"

