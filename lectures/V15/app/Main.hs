{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Main where

data Exp0 = Int0 Int | Add0 Exp0 Exp0

data ExpF x = IntF Int | AddF x x | NegF x 

instance Functor ExpF where
    fmap :: (a -> b) -> ExpF a -> ExpF b
    fmap g (IntF i) = IntF i
    fmap g (AddF x1 x2) = AddF (g x2) (g x2)
    fmap g (NegF x) = NegF (g x)

newtype Mu f = In (f (Mu f))

type Exp1 = Mu ExpF

type Algebra f a = f a -> a

evalExpAlg :: Algebra ExpF Int
evalExpAlg (IntF i) = i
evalExpAlg (AddF x1 x2) = x1 + x2
evalExpAlg (NegF x) = -x

foldMu :: Functor f => Algebra f a -> Mu f -> a
foldMu alg (In x) = alg (fmap (foldMu alg) x)

eval1 :: Exp1 -> Int
eval1 = foldMu evalExpAlg

newtype Val e = Val Int
type ConstExpr = Mu Val

data Add e = Add e e
type AddExpr = Mu Add

ccc :: ConstExpr
ccc = In $ Val 1

aaa :: Mu Add
aaa = In $ Add aaa aaa

infixr :+:
data (f :+: g) e = Inl (f e) | Inr (g e)
type AddConstExpr = Mu (Val :+: Add)

cst' :: Int -> AddConstExpr
cst' n = In $ Inl $ Val n

class sub :<: super where
    inj :: sub a -> super a

instance f :<: f where
    inj :: f a -> f a
    inj = id

instance f :<: (f :+: g) where
    inj = Inl

instance (f :<: g) => f :<: (h :+: g) where
    inj = Inr . inj

data Zero f

instance Functor Zero where
  fmap :: (a -> b) -> Zero a -> Zero b
  fmap = undefined

  



main :: IO ()
main = putStrLn "Hello, Haskell!"
