module Main where
import Test.QuickCheck ((==>), quickCheck)
import qualified Test.QuickCheck.Property

factorList :: Integral a => a -> [a]
factorList x = [k | k <- [2..x], mod x k == 0]

-- I would love to use `Natural`, but quickcheck doesn't like it
smallestFactor1 :: Integral a => a -> a
smallestFactor1 x = head (factorList x)

smallestFactor2 :: Integral t => t -> t -> t
smallestFactor2 x k = if mod x k == 0 then k else smallestFactor2 x (k+1)

prop_smallestFactor :: Integer -> Test.QuickCheck.Property.Property
prop_smallestFactor x = (x >= 2) ==> smallestFactor1 x == smallestFactor2 x 2


main :: IO ()
main = do 
    putStrLn "prop_smallestFactor"
    quickCheck prop_smallestFactor
