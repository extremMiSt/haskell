{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

data a :- b = a :- b 
infixr 2 :-

data SProg x y where
    Noop :: SProg x x
    Pop :: SProg (a :- x) x
    Push :: a -> SProg x (a :- x) 
