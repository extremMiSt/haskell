{-# LANGUAGE FlexibleContexts #-}
module M3546130 where

import Gameplay
    ( PlayerStrategy(PlayerStrategy), StrategyInterface )
import Cards ( Card(rank, suit), Rank(Jack) )
import Game
    ( PlayerState(playerBids, playerTrick, playerHand, playerTrump,
                  playerStack),
      Trick,
      whoTakesTrick,
      legalCard )

import Control.Monad.State.Lazy as State ( MonadState(get) )
import qualified Data.Set as Set
import Data.Maybe (fromJust, isJust)
import Data.List (sort)

------parameters to be tuned, maybe. could also be turned into things in the playerState------
minCardValue :: Rank
minCardValue = Jack

otherPlayers :: Int
otherPlayers = 3

------the strategy------
cleverRobot :: PlayerStrategy
cleverRobot = PlayerStrategy choose bid

------helpers------
bid :: StrategyInterface m => m Int
bid = do 
    playerState <- State.get
    let hand =  Set.elems $ playerHand playerState          -- my hand
        trump =  playerTrump playerState                    -- the current trump, both of these things should be available to me before the bidding starts (modulo the bug I didn't manage to fix...)
        tnp = filter (trumpAndPic trump) hand               -- get only the trumps and pics
    return $ length tnp                                     -- and the amount of that is my bid

trumpAndPic :: Maybe Card -> Card -> Bool
trumpAndPic trump card | isJust trump && suit (fromJust trump) == suit card = True -- trumps are worth a trick
    | rank card >= minCardValue = True                      -- cards with a minimum value are worth a trick
    | otherwise = False

choose :: StrategyInterface m => m Card
choose = do 
    playerState <- State.get
    let trick = playerTrick playerState
        hand =  playerHand playerState
        trump =  playerTrump playerState
        stack = playerStack playerState
        bid = playerBids playerState
        myhand = Set.elems hand
        legalCards = filter (legalCard hand trick) myhand   -- ofc I only want to make legal moves
        curr = div (fromIntegral(length stack)) 4

        lowWin = lowestWin trick trump legalCards
        highWin = lowestWin trick trump legalCards
        highLoss = highestLoss trick trump legalCards
        
    
    -- if I am at exactly the amount of tricks I need, play the largest card that does not win or the lowest card if no loosing card exists
    -- if I have more tricks than needed it really doesn't matter anymore, so might as well play to give other people tricks
    if curr >= bid then
        case highLoss of
            Just x -> return x
            Nothing -> return (minimum legalCards) -- I do not have cards that would loose, play the card with the best chance to still loose

    -- if I have just one more trick to go play the highest winning card
    -- if I need more than one trick, and I am not the last player then play the highest winning card
    else if (curr == bid -1) || (curr < bid) && (length trick <= otherPlayers) then
        case highWin of
            Just x -> return x
            Nothing -> return (minimum legalCards) -- I do not have cards that would win, get rid of lower cards to get more tricks later
    
    -- if I need more than one trick, and I am the last player then play the lowest winning card
    else  {-if (curr < bid) && (length trick == 3)  then -}
        case lowWin of
            Just x -> return x
            Nothing -> return (minimum legalCards) -- I do not have cards that would win, get rid of lower cards to get more tricks later


lowestWin :: Trick -> Maybe Card -> [Card] -> Maybe Card
lowestWin trick trump hand = case filtered of 
    [] -> Nothing
    x -> Just $ minimum filtered
    where 
        filtered = filter (win trick trump) hand

highestWin :: Trick -> Maybe Card -> [Card] -> Maybe Card
highestWin trick trump hand = case filtered of 
    [] -> Nothing
    x -> Just $ maximum filtered
    where 
        filtered = filter (win trick trump) hand

highestLoss :: Trick -> Maybe Card -> [Card] -> Maybe Card
highestLoss trick trump hand = case filtered of 
    [] -> Nothing
    x -> Just $ maximum filtered
    where 
        filtered = filter (not . win trick trump) hand


win :: Trick -> Maybe Card -> Card -> Bool
win trick trump card = ";;MiSt::" == whoTakesTrick ((";;MiSt::", card):trick) trump --haaaaaaaack