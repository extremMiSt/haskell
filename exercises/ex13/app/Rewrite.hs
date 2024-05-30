{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
module Rewrite where
import ALaCarte
import qualified Data.Set as Set

newtype Var e = Var String
    deriving (Show, Functor)

instance Foldable Var where
  foldMap :: Monoid m => (a -> m) -> Var a -> m
  foldMap a2m (Var s) = mempty

instance Traversable Var where
  traverse :: Applicative f => (a -> f b) -> Var a -> f (Var b)
  traverse _a2fb (Var s) = pure (Var s)

data App e = App e e
    deriving (Show, Functor)

instance Foldable App where
  foldMap :: Monoid m => (a -> m) -> App a -> m
  foldMap a2m (App x y) = a2m x <> a2m y

instance Traversable App where
  traverse :: Applicative f => (a -> f b) -> App a -> f (App b)
  traverse a2fb (App x y) = pure App <*> a2fb x <*> a2fb y

data Lam e = Lam String e
    deriving (Show, Functor, Foldable, Traversable)

data Let e = Let String e e
    deriving (Show, Functor, Foldable, Traversable)

instance (Foldable f, Foldable g) => Foldable (f :+: g) where
  foldMap :: (Foldable f, Foldable g, Monoid m) => (a -> m) -> (:+:) f g a -> m
  foldMap a2m (Inl f) = foldMap a2m f
  foldMap a2m (Inr g) = foldMap a2m g

instance (Traversable f, Traversable g) => Traversable (f :+: g) where
  traverse a2tb (Inl f) = Inl <$> traverse a2tb f
  traverse a2tb (Inr g) = Inr <$> traverse a2tb g

rewriteLets :: forall f. (Functor f, App :<: f, Lam :<: f) => Mu (Let :+: f) -> Mu f
rewriteLets = foldMu alg
  where
    alg :: (Let :+: f) (Mu f)-> Mu f
    alg (Inr f) = In f
    alg (Inl (Let var expr body)) = inject $ App (inject $ Lam var body) expr

data LamAnnot e = LamAnnot String Bool e

annotateLam :: forall f. (Traversable f, Functor f, LamAnnot :<: f) => Mu (Lam :+: Var :+: f) -> Mu (Var :+: f)
annotateLam = snd . foldMu alg
  where 

    alg (Inl (Lam var (used,e))) = (Set.delete var used, inject $ LamAnnot var (var `Set.member` used)e)
    alg (Inr (Inl (Var var ))) = (Set.singleton var, inject (Var var))
    alg (Inr (Inr f)) = fmap (In . Inr) $ sequenceA f
    
    go (x,y) = (y,x)






