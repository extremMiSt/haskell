{-# LANGUAGE InstanceSigs #-}
module Main where
import SimplePrelude as S
import Prelude ()
import Test.QuickCheck (quickCheck, Gen (), arbitrary)
import qualified Test.QuickCheck as T
import Test.QuickCheck.Arbitrary (Arbitrary ())
import Test.QuickCheck.Gen (oneof)
import MiniLang (Var)
import Data.Map (Map)

---ex1

instance Monad [] where
    return :: a -> [a]
    return x = [x]
    (>>=) :: [a] -> (a -> [b]) -> [b]
    m >>= f = concatMap f m

guard :: Bool -> [()]
guard True = [()]
guard False = []

prop_guardTrue, prop_guardFalse :: [Integer] -> [Integer] -> Bool
prop_guardTrue xs ys = (xs >> guard True >> ys) == (xs >> ys)
prop_guardFalse xs ys = (xs >> guard False >> ys) == []

---ex2

data These a b = This a | That b | These a b
    deriving (Eq,Show)

instance Semigroup a => Monad (These a) where
    return :: Semigroup a => a1 -> These a a1
    return = That
    (>>=) :: Semigroup a => These a a1 -> (a1 -> These a b) -> These a b
    This  a   >>= _ = This a
    That    x >>= k = k x
    These a x >>= k = case k x of
                          This  b   -> This  (a <> b)
                          That    y -> These a y
                          These b y -> These (a <> b) y

instance (Arbitrary a, Arbitrary b) => Arbitrary (These a b) where
    arbitrary :: (Arbitrary a, Arbitrary b) => Gen (These a b)
    arbitrary = oneof [
            This <$> arbitrary,
            That <$> arbitrary,
            do
                a <- arbitrary
                These a <$> arbitrary
            ]

prop_monad_law_r_id ::  These [Integer] Integer -> Bool
prop_monad_law_r_id m = (m >>= return) == m

prop_monad_law_l_id :: These [Integer] Integer -> Bool
prop_monad_law_l_id a = ((return a) >>= id) == id a

{-prop_monad_law_assoc :: These [Integer] Integer -> Bool
prop_monad_law_assoc m = 
    ((m  >>=  (\x -> g x))  >>=  h)
    ==
    (m  >>=  (\x -> g x    >>=  h))
    where 
        g=id
        h=id-}

--ex3

data State s a = State (s -> (a, s))
exST :: State s a -> s -> (a, s)
exST (State sas) = sas

instance Monad (State s) where
    return :: a -> State s a
    return a = State (\s -> (a, s))
    (>>=) :: State s a -> (a -> State s b) -> State s b
    m >>= f = State (\s -> let (a, s') = exST m s in exST (f a) s')

type Count a = State Int a
incr :: Count ()
incr = State (\i -> ((), i + 1))

type Memory = Map Var Integer

getVar :: Var -> State Memory Integer
getVar v = do 
    return 7

setVar :: Var -> Integer -> State Memory ()
setVar v w = do
    return ()

main :: IO ()
main = do 
    quickCheck prop_guardFalse
    quickCheck prop_guardTrue

    quickCheck prop_monad_law_r_id
    quickCheck prop_monad_law_l_id
    --quickCheck prop_monad_law_assoc
    let x = (These "5")
    undefined


--this exercise sheet slightly broke me