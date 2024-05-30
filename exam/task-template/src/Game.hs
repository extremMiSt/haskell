{-# LANGUAGE TupleSections #-}
module Game where

import qualified Data.Set as Set
import Data.Set (Set)

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map, (!))
import Cards
import qualified Shuffle

import Debug.Trace (trace)

-- start card
twoOfClubs = Card Clubs (Numeric 2)

-- Games
type PlayerName = String

-- * Tricks

-- last card is at the front
type Trick = [(PlayerName, Card)]

emptyTrick :: Trick
emptyTrick = []

trickEmpty :: Trick -> Bool
trickEmpty trick = null trick

trickSize :: Trick -> Int
trickSize trick = length trick

cardsOfTrick :: Trick -> [Card]
cardsOfTrick trick = map snd trick

addToTrick :: PlayerName -> Card -> Trick -> Trick
addToTrick playerName card trick = (playerName, card) : trick

-- from Weber_Maurice
higherCard :: Card -> Card -> Card
higherCard c1 c2 = if cardBeats c1 c2 then c1 else c2

highestCardOfTrick :: Trick -> Card
highestCardOfTrick [] = undefined
highestCardOfTrick trick = 
  let cards = cardsOfTrick trick
  in foldr higherCard (leadingCardOfTrick trick) cards

highestCardOfList :: [Card] -> Card
highestCardOfList [] = undefined
highestCardOfList cs = foldr higherCard (last cs) cs
-- end from Weber_Maurice

leadingCardOfTrick :: Trick -> Card
leadingCardOfTrick trick = snd (last trick)

whoTakesTrick :: Trick -> Maybe Card -> PlayerName
whoTakesTrick [] _ = undefined
whoTakesTrick trick trump =
  case trump of 
    Nothing -> 
      let loop player card [] = player
          loop player card ((player', card') : rest) =
            if (cardBeats card' card )
            then loop player' card' rest
            else loop player card rest
          (player0, card0) : rest = reverse trick
      in loop player0 card0 rest
    (Just t) -> 
      let loop player card [] = player
          loop player card ((player', card') : rest) =
            if (cardBeats card' card || ((suit t == suit card' && ((suit t /= suit card) || (rankBeats (rank card')(rank card))))) )
            then loop player' card' rest
            else loop player card rest
          (player0, card0) : rest = reverse trick
      in loop player0 card0 rest

trickContains :: (Card -> Bool) -> Trick -> Bool
trickContains p trick =
  any p (cardsOfTrick trick)

-- |is it legal to play card given the hand and the partial trick on the table?
legalCard :: Hand -> Trick -> Card -> Bool
legalCard hand trick card =
  containsCard card hand &&
  case trick of
    [] -> True -- if trick is empty, then any card on hand is fine
    _ -> let firstCard = leadingCardOfTrick trick
             firstSuit = suit firstCard
         in  suit card == firstSuit -- ok if suit is followed
             || all ((/= firstSuit) . suit) hand -- ok if no such suit in hand

-- * Games

type PlayerStacks = Map PlayerName (Set Card)
type PlayerHands  = Map PlayerName Hand

data GameState =
  GameState
  { gameStatePlayers :: [PlayerName],
    gameStateHands   :: PlayerHands,
    gameStateStacks  :: PlayerStacks,
    gameStateTrick   :: Trick,
    gameStateTrump   :: Maybe Card,
    gameStateBids    :: Map PlayerName Int,
    gameStatePoints  :: Map PlayerName (Integer,Integer),
    gameStateDealer  :: Maybe PlayerName,
    gameStateDealerList :: [PlayerName],
    gameRounds :: [(Integer,Integer)]
  }
  deriving Show

initialGameState :: [(Integer,Integer)] -> GameState
initialGameState = GameState [] Map.empty Map.empty [] Nothing Map.empty Map.empty {-0-} Nothing []
emptyGameState = GameState [] Map.empty Map.empty []

gameAtBeginning :: GameState -> Bool
gameAtBeginning gameState =
  (trickEmpty (gameStateTrick gameState)) && (all null (Map.elems (gameStateStacks gameState)))

computeNextPlayer :: PlayerName -> [PlayerName] -> PlayerName
computeNextPlayer currentPlayerName playerNames =
  let next [] = head playerNames
      next (playerName:playerNamesRest) =
        if playerName == currentPlayerName
        then head playerNamesRest
        else next playerNamesRest
  in next playerNames

-- determine whose turn it is (assumes at least one player)
nextPlayer :: GameState -> PlayerName
nextPlayer state =
  head (gameStatePlayers state)

-- determine whose turn it is to deal (assumes at least one player)
nextDealer :: GameState -> PlayerName
nextDealer state =
  head (gameStateDealerList state)

playValid :: GameState -> PlayerName -> Card -> Bool
playValid gameState playerName card =
  -- FIXME: validate that the card is valid for the trick
  let hand = gameStateHands gameState ! playerName
      trick = gameStateTrick gameState
  in
  legalCard hand trick card &&
  nextPlayer gameState == playerName

gameOver :: GameState -> Bool
gameOver state = all isHandEmpty $ Map.elems $ gameStateHands state

calculatePoints :: GameState -> Map PlayerName (Integer,Integer)
calculatePoints gameState= 
  let stacks = gameStateStacks gameState
      bids = gameStateBids gameState in
  Map.mapWithKey (\x y -> if div (length y) 4 == (bids ! x) 
    then (div (fromIntegral(length y)) 4, div (fromIntegral(length y)) 4 + 10)
    else (0,0)) 
      stacks

bidOver :: GameState -> Bool
bidOver state = Map.size(gameStateBids state) >= 4

gameRound :: GameState -> [(Integer,Integer)]
gameRound state = (gameRounds state)

players :: GameState -> [PlayerName]
players state = (gameStatePlayers state)

turnOver :: GameState -> Bool
turnOver state = Map.size (gameStateHands state) == trickSize (gameStateTrick state)

data GameEvent =
    HandsDealt (Map PlayerName Hand)
  | TrumpCardTurned Card
  | PlayerBidTurn PlayerName
  | BidChosen PlayerName Int
  | PlayerTurn PlayerName
  | CardPlayed PlayerName Card
  | TrickTaken PlayerName Trick
  | IllegalMove PlayerName
  | DealerChosen PlayerName [PlayerName] [(Integer,Integer)] (Map PlayerName (Integer, Integer)) -- dealer, players, round, points map
  | RoundOver
  | GameOver
  deriving Show

data GameCommand =
    DealHands (Map PlayerName Hand)
  | PlayCard PlayerName Card
  | ChooseDealer PlayerName [PlayerName] [(Integer,Integer)] (Map PlayerName (Integer,Integer))
  | ChooseBid PlayerName Int
  | TurnTrumpCard Card
  deriving Show

takeCard :: PlayerHands -> PlayerName -> Card -> PlayerHands
takeCard playerHand player card =
  Map.alter (fmap (removeCard card)) player playerHand

addToStack :: PlayerStacks -> PlayerName -> [Card] -> PlayerStacks
addToStack playerStack player cards =
  Map.alter (fmap (Set.union (Set.fromList cards))) player playerStack

processGameEvent :: GameEvent -> GameState -> GameState
processGameEvent event state | trace ("processGameEvent " ++ show state ++ " " ++ show event) False = undefined
processGameEvent (DealerChosen dealer players round map) state = 
  state { gameStateDealer = (Just dealer), 
          gameRounds = round,
          gameStatePlayers = rotate (rotateTo dealer (gameStatePlayers state)), 
          gameStatePoints = Map.unionWith (\(t1,p1)(t2,p2) -> (t1+t2,p1+p2)) (gameStatePoints state) map
          }
processGameEvent (HandsDealt hands) state =
  state { gameStatePlayers = Map.keys hands,
              gameStateHands = hands,
              gameStateStacks = Map.fromList (map (, Set.empty) (Map.keys hands)),
              gameStateTrick = emptyTrick, 
              gameStateTrump = Nothing}
processGameEvent (TrumpCardTurned card) state = 
  state { gameStateTrump = Just card }  
processGameEvent (PlayerBidTurn player) state = 
  state { gameStatePlayers = rotateTo player (gameStatePlayers state) }      
processGameEvent (BidChosen player bid) state = 
  state { gameStateBids = Map.insert player bid (gameStateBids state), 
          gameStatePlayers = rotate (rotateTo player (gameStatePlayers state))}
processGameEvent (PlayerTurn player) state =
  state { gameStatePlayers = rotateTo player (gameStatePlayers state) }
processGameEvent (CardPlayed player card) state =
  state { gameStatePlayers = rotate (rotateTo player (gameStatePlayers state)),
              gameStateHands = takeCard (gameStateHands state) player card,
              gameStateStacks = gameStateStacks state,
              gameStateTrick = addToTrick player card (gameStateTrick state) }
processGameEvent (TrickTaken player trick) state =
  state { gameStateStacks = trace (show "addToStack " ++ show player ++ " " ++ show (cardsOfTrick trick) ++ " " ++ show (addToStack (gameStateStacks state) player (cardsOfTrick trick)))
                            (addToStack (gameStateStacks state) player (cardsOfTrick trick)),
          gameStateTrick = emptyTrick }

data PlayerState =
  PlayerState { self :: PlayerName, -- Witzke
                playerHand  :: Hand,
                playerTrick :: Trick,
                playerStack :: [Card],
                playerRound :: [(Integer,Integer)],
                playerTrump :: Maybe Card,
                playerBids :: Int
              }
  deriving Show

initialPlayerState name rounds = PlayerState {
  self = name,
  playerHand = emptyHand,
  playerTrick = emptyTrick,
  playerStack = [],
  playerRound = rounds,
  playerTrump = Nothing, 
  playerBids = 0
  }

playerProcessGameEvent :: PlayerName -> GameEvent -> PlayerState -> PlayerState
playerProcessGameEvent playerName (DealerChosen _ _ round _) state =
  if (fst$head$round) == 1 then 
    (initialPlayerState playerName round) { playerRound = round }
  else 
    state { playerRound = round}
playerProcessGameEvent playerName (HandsDealt hands) state =
  (state) { playerHand = hands ! playerName }
playerProcessGameEvent playerName (TrumpCardTurned card) state =
  state { playerTrump = Just card }
playerProcessGameEvent playerName (PlayerBidTurn playerName') state = state
playerProcessGameEvent playerName (PlayerTurn playerName') state = state
playerProcessGameEvent playerName (BidChosen player bid) state
  | player == playerName =
    state { playerBids = bid }
  | otherwise =
    state
playerProcessGameEvent playerName (CardPlayed player card) state
  | player == playerName =
    state { playerHand = removeCard card (playerHand state),
            playerTrick = addToTrick player card (playerTrick state) }
  | otherwise =
    state { playerTrick = addToTrick player card (playerTrick state) }
playerProcessGameEvent playerName (TrickTaken player trick) state
  | player == playerName =
    state { playerTrick = emptyTrick,
            playerStack = (cardsOfTrick trick) ++ (playerStack state) }
  | otherwise =
    state { playerTrick = emptyTrick}

processGameCommand :: GameCommand -> GameState -> (GameState, [GameEvent])
processGameCommand command state | trace ("processGameCommand " ++ show (gameAtBeginning state) ++ " " ++ show command ++ " " ++ show state) False = undefined
processGameCommand (ChooseDealer dealer players round map) state = 
  let event = DealerChosen dealer players round map
  in (processGameEvent event state, [event])
processGameCommand (DealHands hands) state =
  let event = HandsDealt hands
  in (processGameEvent event state, [event])
processGameCommand (ChooseBid player bid) state = 
  let event1 = BidChosen player bid
      state1 = processGameEvent event1 state
  in if (Map.size (gameStateBids state1) >= 4) 
    then 
      (state1, [event1])
    else 
      let event2 = PlayerBidTurn (nextPlayer state1)
          state2 = processGameEvent event2 state1
      in (state2, [event1, event2])
processGameCommand (TurnTrumpCard card) state =
  let event = TrumpCardTurned card
  in (processGameEvent event state, [event])
processGameCommand (PlayCard player card) state =
  if playValid state player card
  then
    let event1 = CardPlayed player card
        state1 = processGameEvent event1 state
    in  if turnOver state1 then
          let trick = gameStateTrick state1
              trump = gameStateTrump state1
              trickTaker = whoTakesTrick trick trump
              event2 = TrickTaken trickTaker trick
              state2 = processGameEvent event2 state1
              event3 = if gameOver state2
                       then GameOver
                       else PlayerTurn trickTaker
              state3 = processGameEvent event3 state2
          in (state3, [event1, event2, event3])
        else
          let event2 = PlayerTurn (nextPlayer state1)
              state2 = processGameEvent event2 state1
          in (state2, [event1, event2])
  else
    (state, [IllegalMove player, PlayerTurn player])

--------------------------------------------------------------------------------
-- general utility

-- |rotate assumes length of input > 0
rotate :: [a] -> [a]
rotate (x : xs) = xs ++ [x]
rotate [] = undefined

-- |rotate left, assumes length of input > 0
rotateL :: [a] -> [a]
rotateL [] = undefined
rotateL xs  = last xs : init xs

-- |rotateTo assumes target exists in input of length > 0
rotateTo :: Eq a => a -> [a] -> [a]
rotateTo y xs@(x : xs') | x == y = xs
                        | otherwise = rotateTo y (xs' ++ [x])
rotateTo y [] = undefined

-- |read number in given range from terminal
getNumber :: (Num a, Ord a, Read a, Show a) => (a, a) -> IO a
getNumber (lo, hi) = do
  s <- getLine
  let input = read s
  if lo <= input && input <= hi
  then return input
  else
    do putStrLn ("Input must be between " ++ (show lo) ++ " and " ++ (show hi) ++ ". Try again")
       getNumber (lo, hi)
