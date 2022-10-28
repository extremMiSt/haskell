module Main where

data Suit = Spades | Hearts | Diamonds | Clubs
    deriving (Show, Eq)

data Color = Black | Red 
    deriving (Show)

data Rank = Numeric Integer | Jack | Queen | King | Ace
    deriving (Show, Ord, Eq)

data Card = Card { rank::Rank, suit::Suit }
    deriving (Show)

data Hand = Last Card | Other Card Hand



rankBeats :: Rank -> Rank -> Bool
rankBeats r1 r2 = r1 > r2 

cardBeats :: Card -> Card -> Bool
cardBeats card c = suit card == suit c && rankBeats (rank card) (rank c)

color :: Suit -> Color
color Diamonds = Black
color Clubs = Black
color Spades = Red
color Hearts = Red

main :: IO ()
main = do 
    let x = (Card Queen Spades){rank = King}
    print x