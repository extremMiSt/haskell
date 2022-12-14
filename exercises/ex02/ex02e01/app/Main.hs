{-# LANGUAGE TemplateHaskell #-}
module Main where
import Test.QuickCheck ((==>))
import qualified Test.QuickCheck.Property
import Test.QuickCheck.All (quickCheckAll)

undup :: Eq a => [a] -> [a]
undup [] = []
undup (x:xs) = x: undup (filter (neq  x) xs)

prop_undup :: Integer -> Integer -> Integer -> Test.QuickCheck.Property.Property
prop_undup x y z = (x /= y && x/= z && y /= z) ==> (undup [x,y,z,x,y,z] == [x,y,z])

prop_undup_idemp :: [Integer] -> Bool
prop_undup_idemp xs = undup xs == undup (undup xs)

neq :: Eq a => a -> a -> Bool
neq x y = x/=y

prop_neq_eq :: Integer -> Bool
prop_neq_eq x = not (neq x x)

prop_neq_inc ::  Integer -> Bool
prop_neq_inc x = neq x (x+1)

main :: IO ()
main = do
    --print "prop_neq_eq"
    --quickCheck prop_neq_eq
    --print "prop_neq_inc"
    --quickCheck prop_neq_inc
    --print "prop_undup"
    --quickCheck prop_undup
    --print "prop_undup_idemp"
    --quickCheck prop_undup_idemp
    r <- run
    print r

run :: IO Bool
run = $quickCheckAll

    -- runtime of my version is quadratic.
    -- the first has 0 filters applied to it, the second 1 the third 2, then n-th n-1.
    -- the sum of that is well known to be in O(n^2)
