module Main where
import Data.List (sort)
import Test.QuickCheck (quickCheck)

-- | can work on integers, if you want a signature that only works on integers then
    -- maxi::Integer -> Integer -> Integer
    -- would also work. similar for the other functions
maxi :: Ord a => a -> a -> a 
maxi x y | x>y = x
maxi _ y = y -- the linter suggested this, is this the origin of unused rust vars starting with _? 

prop_maxi :: Integer -> Integer -> Bool -- quickcheck does not like working on generic types?
prop_maxi x y = maxi x y == last (sort [x,y])

mini :: Ord a => a -> a -> a
mini x y | x<y = x
mini _ y = y

prop_mini :: Integer -> Integer -> Bool
prop_mini x y = mini x y == head (sort [x,y])

max3 :: Ord a => a -> a -> a -> a
max3 x y z | x>=y,x>=z = x;
max3 x y z | y>=x,y>=z = y;
max3 _ _ z = z; 

prop_max3 :: Integer -> Integer -> Integer -> Bool
prop_max3 x y z = max3 x y z == last (sort [x, y, z])

max3Tupled :: Ord a => (a, a, a) -> a
max3Tupled (x,y,z) | x>=y,x>=z = x;
max3Tupled (x,y,z) | y>=x,y>=z = y;
max3Tupled (_,_,z)  = z;

max3Tup :: Ord a => (a, a, a) -> a
max3Tup (x,y,z) = max3 x y z

prop_max3Tupled :: (Integer, Integer, Integer)  -> Bool
prop_max3Tupled (x,y,z) = max3Tupled (x,y,z) == last (sort [x, y, z])

med :: Ord a => a -> a -> a -> a
med x y z  | x>=y,x>=z,y>=z = y;
med x y z  | x>=y,x>=z,y<=z = z;
med x y z  | y>=x,y>=z,x>=z = x;
med x y z  | y>=x,y>=z,x<=z = z;
med x y _  | x>=y = x; -- z has to be the largest
med _ y _  = y;

betterMed :: Ord a => a -> a -> a -> a
betterMed x y z= max3 (mini x y) (mini y z) (mini x z) 

prop_med :: Integer -> Integer -> Integer -> Bool
prop_med x y z = med x y z == head (tail (sort [x, y, z]))

prop_betterMed :: Integer -> Integer -> Integer -> Bool
prop_betterMed x y z = med x y z == betterMed x y z

main :: IO ()
main = do
    print "prop_maxi"
    quickCheck prop_maxi
    print "prop_mini"
    quickCheck prop_mini
    print "prop_max3"
    quickCheck prop_max3
    print "prop_max3Tupled"
    quickCheck prop_max3Tupled
    print "prop_med"
    quickCheck prop_med
