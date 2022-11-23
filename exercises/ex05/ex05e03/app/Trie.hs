module Trie where

import qualified Data.Map as Map
import Data.Map (empty)


data Trie = Trie Bool (Map.Map Char Trie)
    deriving (Show, Eq)

-- | Returns an empty 'Trie'.
empty :: Trie
empty = Trie False Map.empty

-- | Checks if the given 'String' is a part of the provided 'Trie'.
member :: String -> Trie -> Bool
member = error "TODO"

-- | Merges two 'Trie's.
union :: Trie -> Trie -> Trie
union = error "TODO"

-- | Adds the given prefix to every word inside the 'Trie'.
--
-- > word `member` trie  ==>  (p ++ word) `member` prefix p trie
prefix :: String -> Trie -> Trie
prefix = error "TODO"

-- | Inserts the given 'String' into a 'Trie'.
insert :: String -> Trie -> Trie
insert = error "TODO"

-- | Removes the given 'String' from the 'Trie'.
delete :: String -> Trie -> Trie
delete = error "TODO"

-- | Creates a 'Trie' from a list of 'String's.
fromList :: [String] -> Trie
fromList = error "TODO"
