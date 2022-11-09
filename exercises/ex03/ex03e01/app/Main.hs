module Main where
import Test.QuickCheck (quickCheck, Property)
import Test.QuickCheck.Property ((==>))
import Data.AEq ((~==))

foldrFilter ::(a -> Bool) -> [a] -> [a]
foldrFilter f l = foldr (keep f) [] l
    where 
    keep :: (a -> Bool) -> a -> [a] -> [a]
    keep fn x xs  = if fn x then x:xs else xs

prop_foldrFilter_true :: [Integer] -> Bool
prop_foldrFilter_true a = foldrFilter (const True) a == a

remdups :: (Eq a) => [a] -> [a]
remdups l = foldr (keep) [] l
    where 
    keep :: (Eq a) => a -> [a] -> [a]
    keep x [] = [x]
    keep x xs = if head xs == x then xs else x:xs

prop_remdups :: Integer -> Integer -> Integer -> Property
prop_remdups a b c = (a /= b && b/=c && a /= c) ==> remdups [a,a,a,b,b,c,a] == [a,b,c,a]

avg :: [Double] -> Double
avg list = foldr (+) 0.0 list / fromIntegral (length list)

prop_avg :: Double -> Double -> Double -> Bool
prop_avg a b c = avg [a,b,c] ~== (a+b+c)/3


foldlRec :: (t1 -> t2 -> t1) -> t1 -> [t2] -> t1
foldlRec _ n [] = n
foldlRec f n (x:xs) = foldlRec f (n `f` x ) xs

foldlR :: (t1 -> t2 -> t1) -> t1 -> [t2] -> t1
foldlR f n l = foldr acc id l n
    where 
    acc b g x = g (f x b)

main :: IO ()
main = do
    quickCheck prop_foldrFilter_true
    quickCheck prop_remdups
    quickCheck prop_avg


