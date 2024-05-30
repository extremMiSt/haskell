module Main where
    
import Game (whoTakesTrick)
import Cards
import M3546130

testEq :: Eq a => String -> a -> a -> IO ()
testEq name soll is = if is == soll 
    then putStrLn $ name ++" pass"
    else putStrLn $ name ++" FAIL"


main :: IO ()
main = do
    testWhoTakesTrick
    testCleverBid

testWhoTakesTrick :: IO ()
testWhoTakesTrick = do
    testEq "testWhoTakesTrick1" "p1" (whoTakesTrick [("p1", Card Clubs (Numeric 4))] Nothing)
    testEq "testWhoTakesTrick2" "p1" (whoTakesTrick [("p1", Card Clubs (Numeric 4))] Nothing)
    testEq "testWhoTakesTrick3" "p2" (whoTakesTrick [("p1", Card Clubs (Numeric 4)),
                                                     ("p2", Card Clubs (Numeric 5))] Nothing)
    testEq "testWhoTakesTrick4" "p2" (whoTakesTrick [("p1", Card Clubs (Numeric 4)),
                                                     ("p2", Card Clubs (Numeric 5))] Nothing)
    testEq "testWhoTakesTrick5" "p1" (whoTakesTrick [("p1", Card Clubs (Numeric 5)),
                                                     ("p2", Card Clubs (Numeric 4))] (Just $ Card Clubs (Numeric 4)))
    testEq "testWhoTakesTrickExam" "JIMBO" (whoTakesTrick [("Robo-2", Card Hearts (Numeric 2)),
                                                     ("Robo-1", Card Clubs Ace),
                                                     ("JIMBO", Card Hearts (Numeric 9)),
                                                     ("Robo-3", Card Clubs (Numeric 5))] (Just $ Card Hearts (Numeric 10)))
    testEq "testWhoTakesTrickExamReverse" "JIMBO" (whoTakesTrick [("Robo-3", Card Clubs (Numeric 5)),
                                                     ("JIMBO", Card Hearts (Numeric 9)),
                                                     ("Robo-1", Card Clubs Ace),
                                                     ("Robo-2", Card Hearts (Numeric 2))] (Just $ Card Hearts (Numeric 10)))

testCleverBid :: IO ()
testCleverBid = do 
    testEq "testCleverBid1" True (trumpAndPic (Just $ Card Clubs (Numeric 2)) (Card Clubs (Numeric 2)))
    testEq "testCleverBid2" False (trumpAndPic (Just $ Card Clubs (Numeric 2)) (Card Spades (Numeric 2)))
    testEq "testCleverBid3" True (trumpAndPic (Just $ Card Clubs (Numeric 2)) (Card Spades Ace))
    testEq "testCleverBid4" True (trumpAndPic (Just $ Card Clubs (Numeric 2)) (Card Spades minCardValue))