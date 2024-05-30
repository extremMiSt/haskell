{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module ALaCarte where
import Test.QuickCheck (Arbitrary (arbitrary), Gen, oneof)

newtype Mu f = In (f (Mu f))

type Algebra f a = f a -> a

foldMu :: Functor f => Algebra f a -> Mu f -> a
foldMu alg (In x) = alg (fmap (foldMu alg) x)

newtype Val e = Val Int
  deriving (Show)

type ConstExpr = Mu Val

instance Functor Val where
  fmap _ (Val i) = Val i

data Add e = Add e e


type AddExpr = Mu Add

infixr 9 :+:

data (f :+: g) e = Inl (f e) | Inr (g e)
  deriving Show

type AddConstExpr = Mu (Val :+: Add)

instance Functor Add where
  fmap f (Add x y) = Add (f x) (f y)

instance (Functor f, Functor g) => (Functor (f :+: g)) where
  fmap h (Inl e) = Inl (fmap h e)
  fmap h (Inr e) = Inr (fmap h e)

class Functor f => Eval f where
  evalAlgebra :: f Int -> Int

instance Eval Val where
  evalAlgebra (Val i) = i

instance Eval Add where
  evalAlgebra (Add x y) = x + y

instance (Eval f, Eval g) => Eval (f :+: g) where
  evalAlgebra (Inl x) = evalAlgebra x
  evalAlgebra (Inr y) = evalAlgebra y

eval :: Eval f => Mu f -> Int
eval = foldMu evalAlgebra

newtype Neg e = Neg e
  deriving (Functor)

instance Eval Neg where
  -- evalAlgebra :: Neg Int -> Int
  evalAlgebra (Neg x) = -x

type AddConstNegExpr = Mu (Val :+: (Add :+: Neg))

class sub :<: super where
  inj :: sub a -> super a

instance f :<: f where
  inj = id

instance {-# OVERLAPPING #-} f :<: (f :+: g) where
  inj = Inl

instance (f :<: g) => f :<: (h :+: g) where
  inj = Inr . inj

inject :: (g :<: f) => g (Mu f) -> Mu f
inject = In . inj

val :: (Val :<: f) => Int -> Mu f
val x = inject (Val x)

add :: (Add :<: f) => Mu f -> Mu f -> Mu f
add x y = inject (Add x y)

neg :: (Neg :<: f) => Mu f -> Mu f
neg x = inject (Neg x)

data Mul e = Mul e e
  deriving (Functor)

instance Eval Mul where
  evalAlgebra (Mul x y) = x * y

mul :: (Mul :<: f) => Mu f -> Mu f -> Mu f
mul x y = inject (Mul x y)

type AddConstNegMulExpr = Mu (Val :+: (Add :+: (Neg :+: Mul)))

class Render f where
  render :: Render g => f (Mu g) -> String

pretty :: Render f => Mu f -> String
pretty (In x) = render x

instance Render Val where
  render (Val i) = show i

instance Render Add where
  render (Add x y) = pretty x ++ " + " ++ pretty y

instance Render Mul where
  render (Mul x y) = pretty x ++ " * " ++ pretty y

instance (Render f, Render g) => Render (f :+: g) where
  render (Inl x) = render x
  render (Inr y) = render y

instance Render Neg where
  render (Neg x) = '-' : pretty x

data Term f a
  = Pure a
  | Impure (f (Term f a))

instance Functor f => Functor (Term f) where
  fmap h (Pure a) = Pure (h a)
  fmap h (Impure ftf) = Impure (fmap (fmap h) ftf)

thenTerm :: Functor f => Term f a -> (a -> Term f b) -> Term f b
thenTerm (Pure a) g = g a
thenTerm (Impure ftfa) g = Impure (fmap (`thenTerm` g) ftfa)

instance Functor f => Applicative (Term f) where
  pure = Pure
  ag <*> ax = ag `thenTerm` (\g -> ax `thenTerm` (Pure . g))

instance Functor f => Monad (Term f) where
  (>>=) = thenTerm

data Zero a

data One a = One

newtype Const e a = Const e

instance Functor Zero where
  fmap _ z = case z of {}

instance Functor One where
  fmap _ One = One

data Incr t = Incr Int t

newtype Recall t = Recall (Int -> t)

newtype Clear t = Clear t

data Put t = Put Int t

instance Functor Incr where
  fmap f (Incr x t) = Incr x (f t)

instance Functor Recall where
  fmap f (Recall h) = Recall (f . h)

instance Functor Clear where
  fmap f (Clear t) = Clear (f t)

instance Functor Put where
  fmap f (Put x t) = Put x (f t)

injectT :: (g :<: f) => g (Term f a) -> Term f a
injectT = Impure . inj

incr :: (Incr :<: f) => Int -> Term f ()
incr x = injectT (Incr x (Pure ()))

recall :: (Recall :<: f) => Term f Int
recall = injectT (Recall Pure)

clear :: (Clear :<: f) => Term f ()
clear = injectT (Clear (Pure ()))

put :: (Put :<: f) => Int -> Term f ()
put x = injectT (Put x (Pure ()))

foldTerm :: Functor f => (a -> b) -> (f b -> b) -> Term f a -> b
foldTerm f _alg (Pure a) = f a
foldTerm f alg (Impure t) = alg (fmap (foldTerm f alg) t)

newtype Mem = Mem Int
  deriving (Show)

class Functor f => Run f where
  runAlgebra :: f (Mem -> (a, Mem)) -> (Mem -> (a, Mem))

instance Run Incr where
  runAlgebra (Incr k cont) (Mem i) = cont (Mem (i + k))

instance Run Recall where
  runAlgebra (Recall callback) (Mem i) = callback i (Mem i)

instance Run Clear where
  runAlgebra (Clear cont) (Mem _i) = cont (Mem 0)

instance Run Put where
  runAlgebra (Put k cont) (Mem _i) = cont (Mem k)

instance (Run f, Run g) => Run (f :+: g) where
  runAlgebra (Inl r) = runAlgebra r
  runAlgebra (Inr r) = runAlgebra r

run :: Run f => Term f a -> Mem -> (a, Mem)
run = foldTerm (,) runAlgebra


instance (Arbitrary (f a), Arbitrary (g a)) => Arbitrary ((f :+: g) a) where
  arbitrary = oneof [Inl <$> arbitrary , Inr <$> arbitrary]

instance (Functor f, Arbitrary (f (Mu f))) => Arbitrary (Mu f) where
  arbitrary =  In <$> arbitrary
  
instance Arbitrary (Val e) where
  arbitrary = Val <$> arbitrary

instance Arbitrary e => Arbitrary (Add e) where
  arbitrary = do 
    e1 <- arbitrary
    Add e1 <$> arbitrary

instance Arbitrary e => Arbitrary (Neg e) where
  arbitrary = Neg <$> arbitrary

instance Arbitrary e => Arbitrary (Mul e) where
  arbitrary = do
    e <- arbitrary
    Mul e <$> arbitrary

class ArbitraryChoices a where
  arbitraryChoices :: [Gen a]

instance (ArbitraryChoices (f a), ArbitraryChoices (g a)) => ArbitraryChoices ((f :+: g) a) where
  arbitraryChoices = 
    let left = arbitraryChoices
        right = arbitraryChoices
    in map (fmap Inl) left ++ map (fmap Inr) right