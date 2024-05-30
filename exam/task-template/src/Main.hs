module Main where

-- command line parsing
import Options.Applicative
import Data.Semigroup ((<>))

import qualified Gameplay as G
import qualified Shuffle
import qualified Cards
--import qualified MyStrategy as S

import qualified Tournament as T


data CmdLineArgs = CmdLineArgs
  { strategicPlayers :: [StrategicPlayers], gameRounds :: [Integer] }

data StrategicPlayers = PlayAlong String | PlayInteractive String

playStrategy = PlayAlong <$> strOption
  (  long "playAlong"
  <> short 'p'
  <> metavar "PLAYER"
  <> help "Select strategy player" )

playInteractive = PlayInteractive <$> strOption
  (  long "playInteractive"
  <> short 'i'
  <> metavar "PLAYER"
  <> help "Select interactive player" )

rounds = option auto
          ( long "rounds"
         <> help "custom rounds"
         <> showDefault
         <> value [1,2,3,4,5,6,7,8,9,10,11,12,13]
         <> metavar "[x1,x2,..]" )


cmdLineArgs :: Parser CmdLineArgs
cmdLineArgs = CmdLineArgs
  <$> many (playStrategy <|> playInteractive) <*> rounds

main :: IO ()
main = go =<< execParser opts
  where
    opts = info (cmdLineArgs <**> helper)
      ( fullDesc
     <> progDesc "Run the game of Oh Hell with 4 players (can be interactive)"
     <> header "oh hell - run the game of Oh Hell" )

go :: CmdLineArgs -> IO ()
go args =
  if length (strategicPlayers args) > 4 then do
    putStrLn "You can't specify more than 4 players for Oh Hell"
    return ()
  else if not $ roundsVerify (gameRounds args) then do
    putStrLn "cards per round need to be between 1 and 13 inclusively"
    return ()
  else 
    do
    let createPlayer sp = case sp of
          PlayAlong n -> G.makePlayer n G.playAlongStrategy
          PlayInteractive n -> G.makePlayer n G.playInteractive
        players = map createPlayer (strategicPlayers args)
        n = length players
        padding = map (\i -> G.makePlayer ("Robo-" ++ show i) G.playAlongStrategy) [1, 2, 3, 4]
        group = players ++ (take (4-n) padding)
    G.runGame (Shuffle.shuffleRounds 10 Cards.deck) group (zip [1..] (gameRounds args))
    return ()

roundsVerify :: (Ord a, Num a) => [a] -> Bool
roundsVerify [] = True
roundsVerify (r:rs) = if r > 0 && r <=13
  then roundsVerify rs
  else False




