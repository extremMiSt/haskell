{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FunctionalDependencies #-}

module GADTs where

data Empty
data Safe

data List a b where
    Nil :: List a Empty
    Cons :: a -> List a b -> List a Safe

instance Show a => Show (List a b) where
    show :: Show a => List a b -> String
    show Nil = "[]"
    show (Cons a b) = "["++ show a ++ showi b ++ "]"

showi :: Show a => List a b -> String
showi Nil = ""
showi (Cons a b) = ", " ++ show a ++ showi b

safeHead :: List a Safe -> a
safeHead (Cons a b) = a

safeLast :: List a Safe -> a
safeLast (Cons a Nil) = a
safeLast as@(Cons a b) = safeLast as

--safeAppend :: List a b -> List a c -> List a c
--safeAppend Nil Nil = Nil::List a Empty
--safeAppend Nil (Cons a b) = Cons a b
--safeAppend (Cons a b) Nil = Cons a b
--safeAppend (Cons a b) c = Cons a (safeAppend b c)

class Append x y z | x y -> z where
    safeAppend' :: List a x -> List a y -> List a z

instance Append Empty Empty Empty where
    safeAppend' :: List a Empty -> List a Empty -> List a Empty
    safeAppend' _ _ = Nil

instance Append Safe Empty Safe where
    safeAppend' :: List a Safe -> List a Empty -> List a Safe
    safeAppend' as _ = as

instance Append Empty Safe Safe where
    safeAppend' :: List a Empty -> List a Safe -> List a Safe
    safeAppend' _ as = as

instance Append Safe Safe Safe where
    safeAppend' :: List a Safe -> List a Safe -> List a Safe
    safeAppend' = go 
        where 
            go :: List a b -> List a Safe -> List a Safe
            go Nil bs = bs
            go (Cons a b) bs = Cons a (go b bs)

main :: IO ()
main = do 
    let nonempty = Cons 15 (Cons 2023 Nil)
    print nonempty
    let sh1 = safeHead nonempty
    --let ush1 = safeHead Nil
    let sl1 = safeLast nonempty
    --let usl1 = safeLast Nil

    let sa1 = safeAppend' Nil Nil
    --let sat1 = safeHead sa1

    let sa2 = safeAppend' Nil nonempty
    let sat2 = safeHead sa2

    let sa3 = safeAppend' nonempty Nil
    --let sat3 = safeHead sa3  --??

    let sa4 = safeAppend' nonempty nonempty
    let sat4 = safeHead sa4



    putStrLn "done"