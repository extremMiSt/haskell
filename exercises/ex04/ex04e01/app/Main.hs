{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Monoid law, left identity" #-}
{-# HLINT ignore "Monoid law, right identity" #-}
module Main where
import Test.QuickCheck (quickCheckAll, Arbitrary (arbitrary), quickCheck)
import Test.QuickCheck.Gen (Gen)
import System.Posix.Internals (newFilePath)
 
    --Ex01

iterate :: (a->a) -> a -> [a]
iterate f a = a : Main.iterate f (f a)

cycle :: [a] -> [a]
cycle l = ls
    where ls = l ++ ls

    --Ex02

--foldl cannot terminate according to its documentation

foldrLazy :: [Integer]
foldrLazy = foldr (:) [] [1..] 

--foldrShort :: Bool
--foldrShort = foldr ((&&).(<(10::Integer))) True [1..]

    --Ex03

data Vector2D  = Vector2D {x::Integer, y::Integer}

instance Eq Vector2D where
    (==) :: Vector2D -> Vector2D -> Bool
    Vector2D x1 y1 == Vector2D x2 y2 = x1==x2 && y1==y2

instance Show Vector2D where
    show :: Vector2D -> String
    show v = "(" ++ show (x v) ++ "," ++ show (y v) ++ ")"

instance Num Vector2D where
    (+) :: Vector2D -> Vector2D -> Vector2D
    Vector2D x1 y1 + Vector2D x2 y2 = Vector2D (x1+x2) (y1+y2)
    (*) :: Vector2D -> Vector2D -> Vector2D
    Vector2D x1 y1 * Vector2D x2 y2 = Vector2D (x1*x2) (y1*y2)
    abs :: Vector2D -> Vector2D
    abs (Vector2D x y) = Vector2D (abs x) (abs y)
    signum :: Vector2D -> Vector2D
    signum (Vector2D x y) = Vector2D (signum x) (signum y)
    fromInteger :: Integer -> Vector2D
    fromInteger n = Vector2D n n
    (-) :: Vector2D -> Vector2D -> Vector2D
    Vector2D x1 y1 - Vector2D x2 y2 = Vector2D (x1-x2) (y1-y2)

    --Ex04


newtype Sum = Sum Integer
    deriving (Eq, Show)

instance Semigroup Sum where
    (<>) :: Sum -> Sum -> Sum
    Sum x <> Sum y = Sum (x + y)

instance Monoid Sum where
    mempty :: Sum
    mempty = Sum 0

--instance Arbitrary Sum where
-- arbitrary :: Test.QuickCheck.Gen.Gen Sum
--  arbitrary = do Sum <$> (arbitrary :: Gen Integer)


instance Arbitrary Sum where
  arbitrary :: Test.QuickCheck.Gen.Gen Sum
  arbitrary = do 
    x <- (arbitrary :: Gen Integer)
    return (Sum x)

prop_sum_asoc :: Sum -> Sum -> Sum -> Bool
prop_sum_asoc a b c = (a<>b)<>c == a<>(b<>c)

prop_sum_neutral :: Sum -> Bool
prop_sum_neutral a = a == mempty <> a && a == a <> mempty

newtype Product = Product Integer
    deriving (Eq, Show)

instance Semigroup Product where
    (<>) :: Product -> Product -> Product
    Product x <> Product y = Product (x * y)

instance Monoid Product where
    mempty :: Product
    mempty = Product 1

instance Arbitrary Product where
  arbitrary :: Test.QuickCheck.Gen.Gen Product
  arbitrary = do Product <$> (arbitrary :: Gen Integer)

prop_prod_asoc :: Product -> Product -> Product -> Bool
prop_prod_asoc a b c = (a<>b)<>c == a<>(b<>c)

prop_prod_neutral :: Product -> Bool
prop_prod_neutral a = a == mempty <> a && a == a <> mempty

foldMap :: Monoid m => (a -> m) -> [a] -> m
foldMap f = foldr g nil 
    where 
        nil = mempty
        g a m = f a <> m

main :: IO ()
main = do 
    print (take 5 (Main.iterate (2+) (1::Integer)))
    print (take 5 foldrLazy)
    --print foldrShort
    quickCheck prop_sum_asoc
    quickCheck prop_sum_neutral
    quickCheck prop_prod_asoc
    quickCheck prop_prod_neutral




-----------------------------------------------------------------------------
-- Generate a function to check all properties.
--
-- Has to go at the very bottom of the file!

return []

checkAll :: IO Bool
checkAll = $quickCheckAll