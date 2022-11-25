module TrieMap where

import qualified Data.Map as Map
import Data.Map
import Data.Maybe


data TrieMap a = TrieMap (Maybe a) (Map.Map Char (TrieMap a))
    deriving (Show, Eq)

-- | Returns an empty 'Trie'.
empty :: TrieMap a
empty = TrieMap Nothing Map.empty

-- | Checks if the given 'String' is a part of the provided 'Trie'.
member :: String -> TrieMap a -> Maybe a
member [] t@(TrieMap b _) = b
member (a:s) t@(TrieMap _ m) | isNothing (m !? a) = Nothing
member (a:s) t@(TrieMap _ m) = TrieMap.member s (fromJust (m !? a))

get :: String -> TrieMap a -> Maybe a
get = TrieMap.member

-- | Merges two 'Trie's.
union :: TrieMap a -> TrieMap a-> TrieMap a
union t1@(TrieMap b1 m1) t2@(TrieMap b2 m2) = TrieMap b1 m
    where
        m = unionWith TrieMap.union m1 m2

-- | Adds the given prefix to every word inside the 'Trie'.
--
-- > word `member` TrieMap ==>  (p ++ word) `member` prefix p TrieMap
--prefix :: String -> TrieMap a-> TrieMap a
--prefix [] t = t
--prefix (x:xs) t = TrieMap Nothing (Map.singleton x (fromString Nothing xs))

fromString :: a -> String -> TrieMap a
fromString v [] = TrieMap (Just v) Map.empty
fromString v (x:xs) = TrieMap Nothing (Map.singleton x (fromString v xs))

-- | Inserts the given 'String' into a 'Trie'.
insert :: a -> String -> TrieMap a -> TrieMap a
insert v s = TrieMap.union (fromString v s)

-- | Creates a 'Trie' from a list of 'String's.
fromList :: [(a, String)] -> TrieMap a
fromList = Prelude.foldr (TrieMap.union . uncurry fromString) TrieMap.empty

-- | Removes the given 'String' from the 'Trie'.
delete :: String -> TrieMap a -> TrieMap a
delete [] t@(TrieMap b m) = TrieMap Nothing m
delete (x:xs) t@(TrieMap b m) | isNothing (m !? x) = t
delete (x:xs) t@(TrieMap b m) | isJust (m !? x) = TrieMap b (Map.insert x (TrieMap.delete xs (fromJust(m !? x))) m)
