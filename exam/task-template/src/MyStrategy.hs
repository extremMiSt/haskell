module MyStrategy where
{-
import Data.List (findIndex)
import Data.Maybe (fromJust)

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map, (!))
import qualified Data.Set as Set
import Data.Set (Set)

import Control.Monad.State.Lazy as State
import Control.Monad.State.Lazy (State, StateT)

import Cards
import Game (PlayerHistory,
             playerHand, playerTrick, playerHistory,
             legalCard, 
             trickEmpty, trickSize, trickContains, cardsOfTrick, leadingCardOfTrick)
import Gameplay


nHighBound :: Int
nHighBound = 6

nLowBound :: Int
nLowBound = 3

cardsOfHistory :: PlayerHistory -> [Card]
cardsOfHistory = concatMap (cardsOfTrick . snd)

cardsBySuit :: [Card] -> Map Suit (Set Card)
cardsBySuit =  foldr (\card -> Map.adjust (Set.insert card) (suit card))
                     (Map.fromList [(suit, Set.empty) | suit <- allSuits])

allCardsBySuit :: Map Suit (Set Card)
allCardsBySuit = cardsBySuit deck

allHearts = allCardsBySuit ! Hearts
allSpades = allCardsBySuit ! Spades
allClubs = allCardsBySuit ! Clubs
allDiamonds = allCardsBySuit ! Diamonds

-- versatile robo player that considers the history and all that
strategy :: PlayerStrategy
strategy =
  PlayerStrategy $ do
  playerState <- State.get
  let trick   = playerTrick playerState
      hand    = playerHand playerState
      history = playerHistory playerState

      legalCards = Set.filter (legalCard hand trick) hand
      highestCard = Set.findMax legalCards
      mHighestHeart = Set.lookupMax $ Set.filter ((== Hearts) . suit) legalCards

      firstCard = leadingCardOfTrick trick
      firstSuit = suit firstCard
      firstRank = rank firstCard

      hCards = cardsOfHistory history
      historyBySuit :: Map Suit (Set Card)
      historyBySuit = cardsBySuit hCards
      handBySuit :: Map Suit (Set Card)
      handBySuit = cardsBySuit (Set.elems hand)

      -- rules for leading a trick
      -- * do not lead with Hearts if it can be avoided
      -- * do not lead with Q of Spades
      -- * do not lead with K or A of Spades unless you hold Q of Spades is unavailable
      -- * lead with a high card if
      --    + > H cards of the suit are available and
      --    + everybody has followed the suit so far
      -- * lead with a low card if
      --    + <= H cards of the suit are available and
      --    + as least L higher cards are available (at most one lower card is available)
      leadWeight :: Map Suit (Set Card) -> Map Suit (Set Card) -> Card -> Int
      leadWeight inHand inHistory card =
        let crank = rank card
            unavailable suit = Set.union (inHand ! suit) (inHistory ! suit)
            available suit = Set.difference (allCardsBySuit ! suit) (unavailable suit)
            queenOfSpades = Card Spades Queen
        in
        case suit card of
          Hearts ->
            - fromJust (findIndex (== crank) allRanks)
          Spades | crank == Queen -> -50
                 | crank > Queen && Set.member queenOfSpades (available Spades) -> -40
          csuit  ->
            let cpos = fromJust (findIndex (== crank) allRanks) in
            if Set.size (available csuit) > nHighBound then
              cpos
            else
              13 - cpos
          
      -- rules for playing a non-leading card
      -- * if you don't have to follow suit
      --      + play Q of Spades
      --      + play a highly ranked Hearts
      --      + play a highly ranked card of the suit with the least number of cards on hand
      -- * if you have to follow suit
      --      + do not play Q of Spades
      --      + if the suit is Hearts, try to ensure that someone else takes the trick
      --      + if more than H cards of the suit are available, play a high card
      --      + if fewer the H cards of the suit are available, play a low card
      playWeight inHand inHistory card =
        let crank = rank card
            trickBySuit = cardsBySuit (cardsOfTrick trick)
            cpos = fromJust (findIndex (== crank) allRanks)
            unavailable suit = Set.union (inHand ! suit) (inHistory ! suit)
            available suit = Set.difference (allCardsBySuit ! suit) (unavailable suit)
        in
        if Set.member firstSuit (Set.map suit legalCards) then
          -- must follow suit
          case suit card of
            Spades | crank == Queen -> -50

            Hearts ->
              let heartsInTrick = trickBySuit ! Hearts
                  highestHeartInTrick = Set.findMax heartsInTrick
                  currentlyAvailable = Set.difference (available Hearts) heartsInTrick
              in
              if card < highestHeartInTrick then
                13 + cpos
              else if Set.size (Set.filter (> card) currentlyAvailable) > 1 + trickSize trick then
                cpos
              else
                - cpos
                
            csuit ->
              let currentlyAvailable = Set.difference (available csuit) (trickBySuit ! csuit)
              in
              if Set.size currentlyAvailable > nHighBound then
                cpos
              else
                13 - cpos
        else
          -- play any card
          case suit card of
            Spades | crank == Queen -> 50

            Hearts -> 30 + cpos

            csuit -> (2 * cpos) `div` Set.size (inHand ! csuit)
        
  -- we say a card is *unavailable* if you hold it or it's in the history
  if trickEmpty trick 
    then
      let weightedHand = Set.map (\c -> (leadWeight handBySuit historyBySuit c, c)) legalCards in
      return (snd (Set.findMax weightedHand))
    else
      let weightedHand = Set.map (\c -> (playWeight handBySuit historyBySuit c, c)) legalCards in
      return (snd (Set.findMax weightedHand))
      -}
