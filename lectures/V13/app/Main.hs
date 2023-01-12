{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveFunctor #-}
module Main where
import GHC.TypeLits (Div)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Control.Monad.Trans.Reader (ask, ReaderT (runReaderT))
import Control.Monad.Trans.State (StateT)
import Control.Monad.Trans.Identity (IdentityT(runIdentityT))

--Comp :: (* -> *)->(* -> *)->(* -> *)
newtype Comp f g a = Comp {exComp :: f (g a)}

instance (Functor f, Functor g) => Functor (Comp f g) where
    fmap :: (Functor f, Functor g) => (a -> b) -> Comp f g a -> Comp f g b
    fmap h (Comp fga) = Comp $ fmap (fmap h) fga  

instance (Applicative f, Applicative g) => Applicative (Comp f g) where
    pure :: (Applicative f, Applicative g) => a -> Comp f g a
    pure a = Comp $ pure (pure a)
    (<*>) :: (Applicative f, Applicative g) => Comp f g (a -> b) -> Comp f g a -> Comp f g b
    Comp fff <*> Comp aaa = Comp $ fmap (<*>) fff <*> aaa

data Term = Var String | Con Integer | Bin Term Op Term
    deriving (Eq, Show)
data Op = Add|Sub|Mul|Div
    deriving (Eq,Show)

newtype Identity a = Identity {runIdentity :: a}
    deriving (Functor, Show)

instance Applicative Identity where
    pure :: a -> Identity a
    pure = Identity
    (<*>) :: Identity (a -> b) -> Identity a -> Identity b
    Identity f <*> Identity a = Identity (f a)

instance Monad Identity where 
    (>>=) :: Identity a -> (a -> Identity b) -> Identity b
    Identity a >>= f = f a

type M = ReaderT [(String, Integer)] (MaybeT Identity) 

applyOp :: Op -> Integer -> Integer -> M Integer
applyOp Add x y = return (x+y)
applyOp Div x y | y == 0 = fail "div 0"
                | otherwise = return (x `div` y)

eval :: Term -> M Integer
eval (Var x) = do
    env <- ask 
    case lookup x env of
        Just i -> return i
        Nothing -> fail "unknown var"
        
eval (Con i) = return i
eval (Bin l o r) = do
    vl <- eval l
    vr <- eval r
    applyOp o vl vr

main :: IO ()
main = do
    print $ runIdentity (runMaybeT (runReaderT (eval (Var "x")) [("x", 41)]))
