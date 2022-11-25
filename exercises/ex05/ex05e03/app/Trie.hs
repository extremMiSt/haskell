{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module Trie where

import qualified Data.Map as Map
import Data.Map
import Data.Maybe
import Test.QuickCheck
import Test.QuickCheck.Gen



data Trie = Trie Bool (Map.Map Char Trie)
    deriving (Show, Eq)

-- | Returns an empty 'Trie'.
empty :: Trie
empty = Trie False Map.empty

-- | Checks if the given 'String' is a part of the provided 'Trie'.
member :: String -> Trie -> Bool
member [] t@(Trie b _) = b
member (a:s) t@(Trie _ m) = isJust (m !? a) && Trie.member s (fromJust (m !? a))

-- | Merges two 'Trie's.
union :: Trie -> Trie -> Trie
union t1@(Trie b1 m1) t2@(Trie b2 m2) = Trie (b1||b2) m
    where
        m = unionWith Trie.union m1 m2

-- | Adds the given prefix to every word inside the 'Trie'.
--
-- > word `member` trie  ==>  (p ++ word) `member` prefix p trie
prefix :: String -> Trie -> Trie
prefix [] t = t
prefix (x:xs) t = Trie False (Map.singleton x (fromString xs))

fromString :: String -> Trie
fromString [] = Trie True Map.empty
fromString (x:xs) = Trie False (Map.singleton x (fromString xs))

-- | Inserts the given 'String' into a 'Trie'.
insert :: String -> Trie -> Trie
insert s = Trie.union (fromString s)

-- | Creates a 'Trie' from a list of 'String's.
fromList :: [String] -> Trie
fromList = Prelude.foldr (Trie.union . fromString) Trie.empty

-- | Removes the given 'String' from the 'Trie'.
delete :: String -> Trie -> Trie
delete [] t@(Trie b m) = Trie False m
delete (x:xs) t@(Trie b m) | isNothing (m !? x) = t
delete (x:xs) t@(Trie b m) | isJust (m !? x) = Trie b te
    where 
        te = if Trie.delete xs (fromJust(m !? x)) == Trie.empty 
                then Map.delete x m
                else Map.insert x (Trie.delete xs (fromJust(m !? x))) m


instance Arbitrary Trie where
  arbitrary :: Gen Trie
  arbitrary = do 
    b <- arbitrary
    m <- arbitrary
    return (Trie b m)

prop_empty_is_empty :: String -> Bool
prop_empty_is_empty s = not (Trie.member s Trie.empty)

prop_insert_on_empty :: String -> Bool
prop_insert_on_empty s = Trie.member s (Trie.insert s Trie.empty)

prop_insert_on_ :: p -> String -> Trie -> Bool
prop_insert_on_ arbitrary s t = Trie.member s (Trie.insert s t)

prop_union_has_both :: String -> String -> Trie -> Trie -> Bool
prop_union_has_both s1 s2 t1 t2 = Trie.member s1 tu && Trie.member s2 tu 
    where tu = Trie.insert s1 t1 `Trie.union` Trie.insert s2 t2

prop_fromList :: [String] -> Bool
prop_fromList l = ca l (Trie.fromList l)
    where 
        ca [] t = True
        ca (s:ss) t = Trie.member s t && ca ss t

prop_delete_deletes :: String -> Trie -> Bool
prop_delete_deletes s t = not (Trie.member s (Trie.delete s (Trie.insert s t)))

--prop_delete_minimal :: String -> Trie -> Property
--prop_delete_minimal s t = not (Trie.member s t) ==> Trie.delete s (Trie.insert s t) == t


-----------------------------------------------------------------------------
-- Generate a function to check all properties.
--
-- Has to go at the very bottom of the file!

return []

checkAll :: IO Bool
checkAll = $quickCheckAll