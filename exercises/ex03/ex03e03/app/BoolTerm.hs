-- Copyright 2022 University of Freiburg
-- Janek Spaderna <janek.spaderna@pluto.uni-freiburg.de>
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use :" #-}
{-# LANGUAGE TemplateHaskell #-}

module BoolTerm where

import Control.Exception (evaluate)
import Control.Monad (void)
import GHC.Generics (Generic)
import Test.QuickCheck

-------------------------------------------------------------------------------
-- BoolTerm definition

data BoolTerm
  = T
  | F
  | Var Char
  | Not BoolTerm
  | Conj BoolTerm BoolTerm
  | Disj BoolTerm BoolTerm
  deriving (Eq, Show, Generic)

type Position = [Integer]

-------------------------------------------------------------------------------
-- Implement the functions here.

pos :: BoolTerm -> [Position]
pos T = [[]]
pos F = [[]]
pos (Var _) = [[]]
pos (Not t) = [[]] ++ [1:x | x <- pos t]
pos (Conj t1 t2) = [[]] ++ [1:x | x <- pos t1] ++ [2:x | x <- pos t2]
pos (Disj t1 t2) = [[]] ++ [1:x | x <- pos t1] ++ [2:x | x <- pos t2]

(|.) :: BoolTerm -> Position -> BoolTerm
(|.) t [] = t
(|.) (Not t) (1:xs) = t |. xs
(|.) (Conj t1 _) (1:xs) = t1 |. xs
(|.) (Conj _ t2) (2:xs) = t2 |. xs
(|.) (Disj t1 _) (1:xs) = t1 |. xs
(|.) (Disj _ t2) (2:xs) = t2 |. xs
(|.) t l = error (show l ++ " not valid for " ++ show t)

replace :: BoolTerm -> BoolTerm -> Position -> BoolTerm
replace _ r [] = r
replace (Not t) r (1:ps) = Not (replace t r ps)
replace (Conj t1 t2) r (1:ps) = Conj (replace t1 r ps) t2
replace (Conj t1 t2) r (2:ps) = Conj t1 (replace t2 r ps)
replace (Disj t1 t2) r (1:ps) = Disj (replace t1 r ps) t2
replace (Disj t1 t2) r (2:ps) = Disj t1 (replace t2 r ps)
replace t _ p = error (show p ++ " not valid for " ++ show t)


-------------------------------------------------------------------------------
-- Properties

-- | All terms should contain Îµ in their set of positions.
prop_pos_empty :: BoolTerm -> Bool
prop_pos_empty t = [] `elem` pos t

-- | Checks that all positions returned from `pos` resolve to valid subterms.
prop_pos_valid :: BoolTerm -> Property
prop_pos_valid t = forAll (elements (pos t)) $ \p ->
  ioProperty $ void $ evaluate (t |. p)

-- | 1. t_pq = (t|p)|q
prop_at_split :: BoolTerm -> Property
prop_at_split t =
  forAll (elements (pos t)) $ \pq ->
    forAll (choose (0, length pq)) $ \n ->
      let (p, q) = splitAt n pq
       in t |. p |. q == t |. pq

-- | 2. (s[t]p)|pq = t|q
prop_replace_split :: BoolTerm -> BoolTerm -> Property
prop_replace_split s t =
  forAll (elements (pos s)) $ \p ->
    forAll (elements (pos t)) $ \q ->
      replace s t p |. (p ++ q) == t |. q

-- | 3. (s[t]p)[r]pq = s[t[r]q]p
prop_replace_twice :: BoolTerm -> BoolTerm -> BoolTerm -> Property
prop_replace_twice s t r =
  forAll (elements (pos s)) $ \p ->
    forAll (elements (pos t)) $ \q ->
      replace (replace s t p) r (p ++ q) == replace s (replace t r q) p

-------------------------------------------------------------------------------
-- Allow quantification over `BoolTerm` values.

instance Arbitrary BoolTerm where
  arbitrary = sized go
    where
      go :: Int -> Gen BoolTerm
      go 0 = oneof [pure T, pure F, Var <$> choose ('a', 'z')]
      go n =
        oneof
          [ Not <$> go (n - 1),
            binary (n - 1) Conj,
            binary (n - 1) Disj
          ]

      binary :: Int -> (BoolTerm -> BoolTerm -> BoolTerm) -> Gen BoolTerm
      binary n f = do
        !a <- invE 0.25 <$> choose (0, 1)
        !b <- invE 0.25 <$> choose (0, 1)
        let rescaled x = go $ round $ x * fromIntegral n / (a + b)
        f <$> rescaled a <*> rescaled b

      invE :: Double -> Double -> Double
      invE lambda u = (-log (1 - u)) / lambda

  shrink = genericShrink


-----------------------------------------------------------------------------
-- Generate a function to check all properties.
--
-- Has to go at the very bottom of the file!

return []

checkAll :: IO Bool
checkAll = $quickCheckAll