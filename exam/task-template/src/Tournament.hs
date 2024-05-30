{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
module Tournament where

import Control.Monad.State.Lazy as State
import Control.Monad.State.Lazy (State, StateT)

import qualified Data.List as List
import Data.List.Split (chunksOf)

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map, (!))

import qualified Cards
import qualified Gameplay as G
import qualified Game
import qualified Shuffle

type ScoreInterface m = (MonadIO m, MonadState ScoreMap m)
type ScoreMap = Map Game.PlayerName Int

start :: [G.Player] -> Int -> IO ScoreMap
start players nRounds =
  let n = length players
      padding = map (\i -> G.makePlayer ("Robo-" ++ show i) G.playAlongStrategy) [1, 2, 3]
      nGroups = (n + 3) `div` 4
      groups = take nGroups $ chunksOf 4 (players ++ padding)
      emptyScore = Map.fromList [(G.playerName player, 0) | player <- players ++ padding ]
  in
    State.execStateT (allRounds groups nRounds) emptyScore

allRounds :: ScoreInterface m => [[G.Player]] -> Int -> m ()
allRounds groups nRounds =
  if nRounds == 0 then
    return ()
  else do
    shuffledCards <- liftIO (Shuffle.shuffleRounds 10 Cards.deck)
    flip mapM_ groups $ \ group -> do
               let perms = List.permutations group in
                 flip mapM_ perms $ \ group_perm -> do
                   score <- liftIO $ G.runGame (return shuffledCards) group_perm [(1,1),(2,2),(3,3),(4,4)]
                   State.modify (Map.unionWith (+) score)
    allRounds groups (nRounds - 1)
