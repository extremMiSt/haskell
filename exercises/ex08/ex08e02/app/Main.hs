{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use join" #-}
module Main where
import Data.Maybe (mapMaybe)
import Test.QuickCheck (Fun, applyFun, quickCheck, applyFun2, applyFun3)


--1)
join :: Monad m => m (m a) -> m a
join mma = do
    ma <- mma
    ma 

--2)
plus :: Integer -> Integer -> Integer
plus = (+)

joinPlus :: Integer -> Integer
joinPlus = join (+)

joinTimes :: Integer -> Integer
joinTimes = join (*)

prop_joinFkn2Int :: Fun (Integer, Integer) Integer -> Integer -> Bool
prop_joinFkn2Int f v = applyFun2 f v v == join (applyFun2 f) v

plus3 :: Num a => a -> a -> a -> a
plus3 a b c = a+b+c

plus4 :: Num a => a -> a -> a -> a -> a
plus4 a b c d= a+b+c+d

joinPlus3 :: Integer -> Integer -> Integer
joinPlus3 = join plus3


joinPlus4 :: Integer -> Integer -> Integer -> Integer
joinPlus4 = join plus4

prop_joinFknInt :: Fun (Integer, Integer, Integer) Integer -> Integer -> Integer -> Bool
prop_joinFknInt f v w = applyFun3 f v v w == join (applyFun3 f) v w

{-
wenn es typt...
dann fasse die ersten 2 parameter zusammen und fülle sie mit dem gleichen wert
join f x y z == f x x y z
dafür müssen der typ des ersten und zweiten paramters aber kompatibel sein
join (:) typt zB nicht, weil a und [a] nicht kompatiblel sind
-}

--3)
dot :: (b -> c) -> (a -> b) -> a -> c
dot = (.)

joinDot :: (a -> a) -> a -> a
joinDot = join (.)

prop_joinDotAppliesTwiceInteger :: Fun Integer Integer -> Integer -> Bool
prop_joinDotAppliesTwiceInteger f a = joinDot (applyFun f) a == applyFun f (applyFun f a)

prop_joinDotAppliesTwiceList :: Fun [Integer] [Integer] -> [Integer] -> Bool
prop_joinDotAppliesTwiceList f a = joinDot (applyFun f) a == applyFun f (applyFun f a)

main :: IO ()
main = do
    quickCheck prop_joinDotAppliesTwiceInteger
    quickCheck prop_joinDotAppliesTwiceList

    quickCheck prop_joinFkn2Int
    quickCheck prop_joinFknInt
