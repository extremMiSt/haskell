{-# LANGUAGE ScopedTypeVariables #-}
module Main where
import Data.Maybe (isJust, fromJust, isNothing)


-- e01


--data  Maybe a  =  Nothing | Just a
--  deriving ( Eq  -- ^ @since 2.01
--          , Ord -- ^ @since 2.01
--          )

isJust' :: Maybe a -> Bool
isJust' Nothing = False
isJust' (Just _) = True

maybe' :: p -> (t -> p) -> Maybe t -> p
maybe' d _ Nothing = d
maybe' _ f (Just a) = f a 

--mapMaybe' :: (a -> Maybe b) -> [a] -> [b]
--mapMaybe' _ [] = []
--mapMaybe' f (x:xs) = apply x ++ mapMaybe' f xs
--    where 
--        apply m | isJust (f m) = [fromJust (f m)]
--                | otherwise = []

mapMaybe' :: (a -> Maybe b) -> [a] -> [b]
mapMaybe' _ [] = []
mapMaybe' f (x:xs) | isJust (f x) = fromJust (f x) : mapMaybe' f xs
                   | otherwise    = mapMaybe' f xs

catMaybes' :: [Maybe b] -> [b] -- just done cause too easy :D
catMaybes' = mapMaybe' id


--  e02

unfoldr' :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr' f s | isNothing (f s) = []
             | otherwise = a : unfoldr' f s'
                where 
                    (a,s') = fromJust (f s)

map' :: (a->b) -> [a] -> [b]
map' f = unfoldr' f'
    where 
        f' [] = Nothing
        f' (x:xs) = Just (f x,xs) 

range' :: Enum a => a -> a -> [a] --why do it any differently?, or rather.. why at all?
range' m n = [m..n]

range'' :: Integer -> Integer -> [Integer]
range'' m n = unfoldr' f m
                where 
                    f s | n < s      = Nothing
                        | otherwise   = Just (s, s+1)

main :: IO ()
main = do 
    print (range'' 0 10)
    print (range'' 10 0)
    