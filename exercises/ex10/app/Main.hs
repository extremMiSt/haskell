{-# LANGUAGE BlockArguments #-}
module Main where
import Parser (Alternative((<|>), many),Parser, lit, empty)
import MiniLangParser (pinteger)
import Control.Monad (replicateM)
import Data.List (genericReplicate)

main :: IO ()
main = putStrLn "Hello, Haskell!"


psepby :: Parser t r -> Parser t sep -> Parser t [r]
psepby p s = psepby1 p s <|> pure []

psepby1 :: Parser t r -> Parser t sep -> Parser t [r]
psepby1 p s = do
    x <- p
    xs <- many do
        _ <- s
        p
    pure (x:xs)

ppali :: Eq r => Parser t r -> Parser t [r]
ppali p = paliEmpty <|> paliOne <|> paliMany
    where
        paliEmpty = pure []
        paliOne = do
            x <- p 
            pure [x]
        paliMany = do
            r1 <- p
            rs <- ppali p
            r2 <- p
            if r1==r2 
                then pure $ r1:rs++ [r2]
                else empty

pcounted :: Parser Char a -> Parser Char [a]
pcounted p = do
    n <- pinteger
    replicateM (fromInteger n) p

pcounted' :: Parser Char a -> Parser Char [a]
pcounted' p = do
    n <- pinteger
    sequence $ genericReplicate n p