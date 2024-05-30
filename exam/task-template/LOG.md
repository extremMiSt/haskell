renamed stack.yaml and stack.yaml.lock to get hls working. I will undo that, but this has only been worked on with cabal!
added Tests.hs, added target 'test' in cabal

1) 
added 1 line in Gameplay.playInteractive#chooseBid to get and show current cards
tested via playthrough, output is there

2)
added gameStateDealerList to Game.GameState
fixed Game.initialGameState with a trailing []
in Game added rotateL
in Game added nextDealer
in Gameplay added nextDealerM
in Gameplay.processGameCommandM#PlayCard in the 'game over but not end of the game' branch: 
    don't just take the next player as dealer but head of Game.GameState.gameStateDealerList
in Gameplay.processGameEventM#DealerChosen rotate the gameStateDealerList 
    with rotateL $ rotateTo cause the very first dealer is chosen at random and we need to deal with that. 
    nicer format, cause that line was too long even for my taste.
in Gameplay.startController add the players to gameStateDealerList
tested via playthrough, seems fine, robos play in ascending order and loop, named players (-i -p) work too
observation: unless a dealer has been chosen, the player to win a trick is the next player to play. 
    the rules do not state what happens in that situation, so that is fine (and was that way before my changes, too).

3) 
added a few examples as test cases for Game.whoTakesTrick in Test.hs
from the tests I can see that tests seem to fail when 
 - there is a trump
 - a trump card is before another higher valued trump card
and in fact Game.whoTakesTrick does not check rank when the new card is a trump
replaced 
        (suit t == suit card')  
    with 
        ((suit t == suit card' && ((suit t /= suit card) || (rankBeats (rank card')(rank card)))))
    which checks that either the trump is beating a non-trump or is beating a trump of lower rank
tested via previously added tests, passes

4) 
provoking/reproducing the error yields
    > Dealer in round 5 is :  olaf!
    > Your hand:
    >   
    > Pick a bid (0-5)
in Gameplay.strategyPlayer#DealerChosen the order of the commands is right. 
from there they are handed to Gameplay.gameController where they are passed to processGameCommandM' and turned into events
which are then announced on the commandline and then given to the players to create more commands.
meaning all the commands get executed before the events get passed to the players.

no idea how to fix that, spent way too much time, moving on

5)
add Main.rounds (the parser)
extend Main.CmdLineArgs for the rounds
add Main.roundsVerify, and use it in Main.go to verify the argument
extend Gameplay.runGame to take the rounds list, pass it to Game.initialGameState
extend Game.GameState to include rounds data, 'fix' signature of Game.initialGameState to accept the rounds
-> with that the rounds data is now available with the (gamestate) monad
remove the old rounds counter and replace it everywhere with the new list and the appropriate successor and accessors, this is neccecary in:
- processGameEvent
- DealerChosen
- gameRound
- playerRound
- ChooseDealer
- initialPlayerState
- playerProcessGameEvent
- processGameEventM
- gameRoundM
- playInteractive
- playAlongStrategy
- strategyPlayer
- processGameCommandM'
- startController
compiles and works through a playthrough, i and p.

6)
change announcement in announceEvent#DealerChosen
tested via playthrough, seems fine

7)
add RoundOver GameEvent.
added handeling of it in playerProcessGameEventM, strategyPlayer, processGameCommandM',ProcessGameEventM, announceEvent, just to not run into missing cases later
change gameStatePoints to include taken tricks and points, fix resulting problems in processGameEvent
    this will also neccecitate a change to the player points maps, and the DealerChosen&ChooseDealer events
    and with that a change to processGameCommandM' and calculatePoints
trigger the RoundOver event in processGameCommandM'#PlayCard if "game over" but when the game is not actually over, 
it is here that i notice that the provided calculation is broken because stacks do not clear 
    first it looked like later rounds just got ignored, but I think that is cause the stacks are just weird
    there is no task for it so I won't be fixing it.
prettyprint in announceEvent for RoundOver and GameOver.

8)
since there is a bug with the stacks not getting cleared i can write for this faulty implementation or the game proper.
-> i will be writing a bot for the proper version of the game

ideas for strategy:
- bidding: every trump and every card with a high enough value in my hands is a potential trick
- get to the number of bid tricks as soon as possible, then avoid any more tricks by playing the highest value card that won't win
- edge cases: 
    - I'm playing the last card of the trick and still need more than 1 trick: play the lowest card that will win
    - I'm playing the last card of the trick and still need exactly 1 trick: play the highest card that will win
        to avoid being forced to take a trick later

this is close to my irl strategy for a nameless family game that is often played at family gatherings and eerily close to oh hell. 
    I do not win often ...
    but it should (I hope) be better than a bot playing the wrong game (the play along chooseCard looks like it's for Hearts still).

implementation:
add file/module M3546130. add it to the cabal file, too
more explanation will be provided as comments in the module
tested it with a playthrough, it at least plays without crashing.

end)
renamed the stack files back to original