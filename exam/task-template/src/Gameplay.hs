{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
module Gameplay where

import Control.Conditional (ifM)
import qualified Control.Monad.Writer (WriterT)
import Control.Monad.Writer as Writer

import Control.Monad.State.Lazy as State
import Control.Monad.State.Lazy (State, StateT)

import Control.Monad as Monad

import qualified Data.Foldable as Foldable
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map, (!))
import qualified Data.Set as Set

import Debug.Trace (trace, traceShowId, traceIO, traceM)

import Cards
import Game hiding (processGameCommandM, processGameEvent, playerProcessGameEvent)
import qualified Shuffle

import System.Random
import qualified Data.IntMap as State

-- different
type StrategyInterface m = (HasPlayerState m, MonadIO m)

data PlayerStrategy
  = PlayerStrategy { chooseCard :: forall m . StrategyInterface m => m Card, chooseBid :: forall m . StrategyInterface m => m Int}

type PlayerInterface m = (MonadIO m, MonadWriter [GameCommand] m)

data PlayerEventProcessor =
  PlayerEventProcessor (forall m . PlayerInterface m =>
                         GameEvent -> m PlayerEventProcessor)

data Player =
  Player
  { playerName :: PlayerName
  , eventProcessor :: forall m . PlayerInterface m => GameEvent -> m Player
  }

type PlayerScore = Map PlayerName Int

-- main entry point
runGame :: IO [Card] -> [Player] -> [(Integer,Integer)]-> IO PlayerScore
runGame getCards players rounds = do
  -- create game state monad on top of IO
  State.evalStateT (startController getCards players) (initialGameState rounds)

type HasGameState m = MonadState GameState m

type GameInterface m = (MonadState GameState m, MonadWriter [GameEvent] m)

type ControllerInterface m = (MonadIO m, MonadState GameState m)

startController :: ControllerInterface m => IO [Card] -> [Player] -> m PlayerScore
startController getCards players = do
  -- setup game state
  let playerNames = map playerName players
  State.modify (\state -> state { gameStatePlayers = playerNames,
                                  gameStateStacks  = Map.fromList (zip playerNames $ repeat Set.empty),
                                  gameStateDealerList = playerNames
                                })
  --chooseDealer, begin Bidding
  i <- liftIO (randomRIO (0, (length playerNames) - 1))
  let dealer = playerNames !! i
  st <- State.get
  gameController players [ChooseDealer dealer playerNames (gameRounds st) Map.empty]

-- state projections

playValidM :: HasGameState m => PlayerName -> Card -> m Bool
playValidM playerName card = do
  state <- State.get
  return (playValid state playerName card)

bidOverM :: HasGameState m => m Bool
bidOverM =
  fmap bidOver State.get

calculatePointsM :: HasGameState m => m (Map PlayerName (Integer,Integer))
calculatePointsM =
  fmap calculatePoints State.get

gameRoundM :: HasGameState m => m [(Integer,Integer)]
gameRoundM =
  fmap gameRound State.get

playersM :: HasGameState m => m [PlayerName]
playersM =
    fmap players State.get

turnOverM :: HasGameState m => m Bool
turnOverM =
  fmap turnOver State.get

nextPlayerM :: HasGameState m => m PlayerName
nextPlayerM =
  fmap nextPlayer State.get

nextDealerM :: HasGameState m => m PlayerName
nextDealerM =
  fmap nextDealer State.get

currentTrickM :: HasGameState m => m Trick
currentTrickM =
  fmap gameStateTrick State.get

currentTrumpM :: HasGameState m => m (Maybe Card)
currentTrumpM =
  fmap gameStateTrump State.get

gameOverM :: HasGameState m => m Bool
gameOverM =
  fmap gameOver State.get

runPlayer :: PlayerInterface m => Player -> GameEvent -> m Player
runPlayer (Player p f) gameEvent = f gameEvent

getPenalties :: HasGameState m => m PlayerScore
getPenalties = do
  gameState <- State.get
  let stacks = gameStateStacks gameState
  return $ Map.map penalty stacks

announceEvent :: ControllerInterface m => GameEvent -> m ()
announceEvent (TrumpCardTurned card) =
  liftIO $ putStrLn ("Trump Card is : " ++ pretty card)
announceEvent (PlayerBidTurn playerName) =
  liftIO $ putStrLn ("Your turn to bid, " ++ playerName ++ "!")
announceEvent (BidChosen name bid) =
  liftIO $ putStrLn (name ++ " chooses bid : " ++ show bid)
announceEvent (DealerChosen dealerName _ round pointsMap) =
  liftIO $ putStrLn ("Dealer in round "++ show (fst$head$round) ++  " with " ++ show (snd$head$round) ++ " cards is :  " ++ dealerName ++ "!")
announceEvent (PlayerTurn playerName) =
  liftIO $ putStrLn ("Your turn, " ++ playerName ++ "!")
announceEvent (CardPlayed playerName card) =
  liftIO $ putStrLn (playerName ++ " plays " ++ pretty card)
announceEvent (TrickTaken playerName trick) =
  liftIO $ putStrLn (playerName ++ " takes the trick:\n" ++ pretty (cardsOfTrick trick) ++ "\n")
announceEvent (IllegalMove playerName) =
  liftIO $ putStrLn (playerName ++ " tried to play an illegal card")
announceEvent (GameOver) = do
  gameState <- State.get
  liftIO $ putStrLn "Game Over"
  let playerPoints = gameStatePoints gameState
  let (winnerName, winnerPoints) = Map.foldrWithKey (\playerName points (winnerName, winnerPoints) ->
                                          if (snd points) >= (snd winnerPoints)
                                          then (playerName, (points))
                                          else (winnerName, (winnerPoints))
                                       ) ("", ((0,0)::(Integer,Integer))) playerPoints
      message = " wins the game"
      stackInfo (playerName, points) = do
        putStr playerName
        putStr " has won "
        putStr (show $ fst points)
        putStr " tricks and "
        putStr (show $ snd points)
        putStrLn " points."
  liftIO $ mapM_ stackInfo (Map.assocs playerPoints)
  liftIO $ putStrLn (winnerName ++ message)
announceEvent (RoundOver) = do
  gameState <- State.get
  liftIO $ putStrLn "Round Over"
  let playerPoints = gameStatePoints gameState
  let stackInfo (playerName, points) = do
        putStr playerName
        putStr " has won "
        putStr (show $ fst points)
        putStr " tricks and "
        putStr (show $ snd points)
        putStrLn " points."
  liftIO $ mapM_ stackInfo (Map.assocs playerPoints)
  liftIO $ putStrLn ""

announceEvent gameEvent = 
  liftIO $ putStrLn (take 10 $ show gameEvent)

gameController :: ControllerInterface m => [Player] -> [GameCommand] -> m PlayerScore
gameController players commands = do
  -- traceM ("** INCOMING COMMANDS " ++ show commands) 
  events <- Writer.execWriterT $ mapM_ processGameCommandM' commands
  mapM_ announceEvent events
  -- st <- State.get
  -- traceM ("** GAMESTATE: " ++ show st)
  -- traceM ("** OUTGOING EVENTS " ++ show events)
  (players', commands') <- Writer.runWriterT $
    mapM (\player -> Foldable.foldlM runPlayer player events) players
  if not (null commands') then
    gameController players' commands'
   else do
    playerPenalties <- getPenalties
    return playerPenalties

processGameCommandM :: GameInterface m => GameCommand -> m ()
processGameCommandM command =
  do gameState <- State.get
     let (gameState', events) = processGameCommand command gameState
     State.put gameState'
     Writer.tell events

-- directly monadic version
processGameCommandM' :: GameInterface m => GameCommand -> m ()
processGameCommandM' (TurnTrumpCard card) =
   processAndPublishEventM (TrumpCardTurned card)
processGameCommandM' (ChooseDealer dealer players round pointsMap) =
  processAndPublishEventM (DealerChosen dealer players round pointsMap)
processGameCommandM' (DealHands playerHands) =
   processAndPublishEventM (HandsDealt playerHands)
processGameCommandM' (ChooseBid playerName bid) = do
  processAndPublishEventM (BidChosen playerName bid)
  bidIsOver <- bidOverM
  nextPlayer <- nextPlayerM
  if bidIsOver then
    processAndPublishEventM (PlayerTurn nextPlayer)
  else
    processAndPublishEventM (PlayerBidTurn nextPlayer)
processGameCommandM' (PlayCard playerName card) =
   do playIsValid <- playValidM playerName card
      if playIsValid then
        do processAndPublishEventM (CardPlayed playerName card)
           turnIsOver <- turnOverM
           if turnIsOver then
             do trick <- currentTrickM
                trump <- currentTrumpM
                let trickTaker = whoTakesTrick trick trump
                processAndPublishEventM (TrickTaken trickTaker trick)
                gameIsOver <- gameOverM
                round <- gameRoundM
                if gameIsOver
                  then do
                    if (length round == 1) 
                    then processAndPublishEventM (GameOver)
                    else do
                      players <- playersM
                      calculatePoints <- calculatePointsM
                      nextDealer <- nextDealerM
                      processAndPublishEventM (RoundOver)
                      processAndPublishEventM (DealerChosen nextDealer players (tail round) calculatePoints)
                else processAndPublishEventM (PlayerTurn trickTaker)
           else
             do nextPlayer <- nextPlayerM
                processAndPublishEventM (PlayerTurn nextPlayer)
      else
        do nextPlayer <- nextPlayerM
           processAndPublishEventM (IllegalMove nextPlayer)
           processAndPublishEventM (PlayerTurn nextPlayer)


processAndPublishEventM :: GameInterface m => GameEvent -> m ()
processAndPublishEventM gameEvent = do
  processGameEventM gameEvent
  Writer.tell [gameEvent]

processGameEventM :: GameInterface m => GameEvent -> m ()
processGameEventM (TrumpCardTurned card) =
   State.modify (\state -> state { gameStateTrump = (Just card) })
processGameEventM (DealerChosen name players round pointsMap) =
  State.modify (\state -> state { 
    gameStateDealerList = rotateL $ rotateTo name (gameStateDealerList state), 
    gameStateDealer = (Just name),
    gameRounds = round,
    gameStatePoints = Map.unionWith (\(t1,p1)(t2,p2) -> (t1+t2,p1+p2)) (gameStatePoints state) pointsMap, 
    gameStateBids = Map.empty})
processGameEventM (BidChosen playerName bid) =
   State.modify (\state -> state { gameStateBids = Map.insert playerName bid (gameStateBids state), gameStatePlayers = rotate (rotateTo playerName (gameStatePlayers state)) })

processGameEventM (PlayerBidTurn playerName) =
  State.modify (\state -> state { gameStatePlayers = rotateTo playerName (gameStatePlayers state) })

processGameEventM (HandsDealt playerHands) =
  do 
    state <- State.get
    let (Just x) = (gameStateDealer state)
    State.put (state { gameStatePlayers = rotateTo x (gameStatePlayers state), 
    gameStateHands = playerHands})

processGameEventM (CardPlayed playerName card) =
  State.modify (\state ->
                   state { gameStatePlayers = rotate (rotateTo playerName (gameStatePlayers state)),
                           gameStateHands   = takeCard (gameStateHands state) playerName card,
                           gameStateStacks  = gameStateStacks state,
                           gameStateTrick   = addToTrick playerName card (gameStateTrick state)
                         })

processGameEventM (PlayerTurn playerName) =
  State.modify (\state -> state { gameStatePlayers = rotateTo playerName (gameStatePlayers state) })

processGameEventM (TrickTaken playerName trick) =
  State.modify (\state -> state { gameStateStacks = (addToStack (gameStateStacks state) playerName (cardsOfTrick trick)),
                                  gameStateTrick = emptyTrick
                                })

processGameEventM (IllegalMove playerName) =
  return ()

processGameEventM (GameOver) =
  return ()

  
processGameEventM (RoundOver) =
  return ()
--------------------------------------------------------------------------------
-- players

type HasPlayerState m = MonadState PlayerState m

modifyHand  f = State.modify (\playerState -> playerState { playerHand = f (playerHand playerState)})
modifyTrick f = State.modify (\playerState -> playerState { playerTrick = f (playerTrick playerState)})
modifyStack :: MonadState PlayerState m => ([Card] -> [Card]) -> m ()
modifyStack f = State.modify (\playerState -> playerState { playerStack = f (playerStack playerState)})

playerProcessGameEventM :: (HasPlayerState m, PlayerInterface m) => PlayerName -> GameEvent -> m ()
playerProcessGameEventM playerName gameEvent = do
  case gameEvent of
    DealerChosen _ _ round _->
      State.modify (\playerState -> playerState { playerRound= round})

    TrumpCardTurned card ->
      State.modify (\playerState -> playerState { playerTrump= (Just card)})

    HandsDealt hands ->
      State.modify (\playerState -> playerState { playerHand = hands ! playerName })

    PlayerTurn turnPlayerName ->
      return ()

    PlayerBidTurn turnPlayerName ->
      return ()

    BidChosen bidPlayerName bid -> do
      when (playerName == bidPlayerName) $
        State.modify (\playerState -> playerState { playerBids= bid})

    CardPlayed cardPlayerName card -> do
      when (playerName == cardPlayerName) $
        modifyHand (removeCard card)
      modifyTrick (addToTrick cardPlayerName card)

    TrickTaken trickPlayerName trick -> do
      when (playerName == trickPlayerName) $
        modifyStack (cardsOfTrick trick ++)
      modifyTrick (const emptyTrick)

    IllegalMove playerName ->
      return ()

    GameOver ->
      return ()

    RoundOver ->
      return ()
   --st <- State.get
   --traceM ("** AFTER PLAYERPROCESSGAMEEVENT " ++ playerName ++ " " ++ show gameEvent ++ ": " ++ show st)

makePlayer :: PlayerName -> PlayerStrategy -> Player
makePlayer playerName strategy =
  strategyPlayer playerName strategy (initialPlayerState playerName [])

strategyPlayer :: PlayerName -> PlayerStrategy -> PlayerState -> Player
strategyPlayer playerName strategy playerState =
  Player playerName $ \ event -> do
    nextPlayerState <- flip State.execStateT playerState $ do
      playerProcessGameEventM playerName event
      playerState <- State.get
      case event of
        DealerChosen name players round pointsMap ->
          when (name == playerName) $ do
            bid <- chooseBid strategy
            shuffledCards <- liftIO (Shuffle.shuffleRounds 10 Cards.deck)
            let shuffledCardsforRound = take (fromInteger(4*(snd$head$round)+1)) shuffledCards
            if (snd$head$round) < 13 then do
              let hands = Map.fromList (zip players (map Set.fromList (Shuffle.distribute (length players) (take ((length shuffledCardsforRound) -1) shuffledCardsforRound))))
              let trump = last shuffledCardsforRound
              Writer.tell [DealHands hands, TurnTrumpCard trump, ChooseBid name bid]
            else do
              let hands = Map.fromList (zip players (map Set.fromList (Shuffle.distribute (length players) (take (length shuffledCardsforRound) shuffledCardsforRound))))
              Writer.tell [DealHands hands, ChooseBid name bid]
        
        PlayerBidTurn name ->
          when (name == playerName) $ do
            bid <- chooseBid strategy
            Writer.tell [ChooseBid playerName bid]

        PlayerTurn turnPlayerName ->
          when (playerName == turnPlayerName) $ do
            card <- chooseCard strategy
            Writer.tell [PlayCard playerName card]

        CardPlayed _ _ ->
          return ()

        TrickTaken _ _ ->
          return ()

        IllegalMove _ ->
          return ()

        GameOver ->
          return ()

        HandsDealt _ ->
          return ()

        TrumpCardTurned _ ->
          return ()

        BidChosen _ _->
          return ()

        RoundOver ->
          return ()

    return (strategyPlayer playerName strategy nextPlayerState)

-- stupid robo player
playAlongStrategy :: PlayerStrategy
playAlongStrategy =
  PlayerStrategy {chooseCard = do
  playerState <- State.get
  let trick = playerTrick playerState
      hand = playerHand playerState
      firstCard = leadingCardOfTrick trick
      firstSuit = suit firstCard
      followingCardsOnHand = Set.filter ((== firstSuit) . suit) hand
  if trickEmpty trick
    then
      return (Set.findMin hand)
    else
      case Set.lookupMin followingCardsOnHand of
        Nothing ->
          return (Set.findMax hand) -- any card is fine, so try to get rid of high hearts
        Just card ->
          return card,        -- otherwise use the minimal following card
  chooseBid = do
    playerState <- State.get
    liftIO (randomRIO (0, fromInteger (fst$head (playerRound playerState)))) } -- no intention to win


-- |interactive player
playInteractive :: PlayerStrategy
playInteractive =
  PlayerStrategy { chooseCard = do
  playerState <- State.get
  let trick = playerTrick playerState
      hand = playerHand playerState
      trump =  playerTrump playerState

  if trickEmpty trick
    then
      liftIO $ putStrLn "You lead the next trick!"
    else
      liftIO $ putStrLn ("Cards on table:\n  " ++ pretty (reverse trick))
  let myhand = Set.elems hand
      ncards = Set.size hand
      legalCards = filter (legalCard hand trick) myhand
      nLegals = length legalCards
  liftIO $ putStrLn ("Your hand:\n  " ++ pretty myhand)
  if nLegals == 1 then do
    liftIO $ putStrLn ("You have no choice.")
    return (head legalCards)
   else do
    liftIO $ putStrLn ("Your choices:\n  " ++ pretty (zip [(1::Integer)..] legalCards))
    liftIO $ putStrLn ("Pick a card (1-" ++ show nLegals ++ ")")
    selected <- liftIO $ getNumber (1,nLegals)
    return (legalCards !! (selected - 1)),

    chooseBid = do
      playerState <- State.get
      liftIO $ putStrLn ("Your hand:\n  " ++ (pretty . Set.elems . playerHand) playerState)
      liftIO $ putStrLn ("Pick a bid (0-" ++ show (snd$head $ playerRound playerState)++ ")")
      liftIO $ getNumber (0,fromInteger (snd$head (playerRound playerState)))
      }


