{-# LANGUAGE InstanceSigs #-}
module Main where

data Entry = X | O | E
    deriving (Eq)

data Line = Line {fstE::Entry, sndE::Entry, trdE::Entry}
    deriving (Eq)

data Tic = Tic{fstL::Line, sndL::Line, trdL::Line}

instance Show Entry where
  show :: Entry -> String
  show X = "X"
  show O = "O"
  show E = " "
  

instance Show Line where
  show :: Line -> String
  show x = show(fstE x) ++ show(sndE x) ++ show(trdE x)

instance Show Tic where
  show :: Tic -> String
  show x = show(fstL x) ++ "\n" ++show(sndL x) ++ "\n" ++ show(trdL x)


hasWon :: Tic -> Entry -> Bool
hasWon t e = fstL t == Line e e e || sndL t == Line e e e || trdL t == Line e e e || 
            fstL (transpose t) == Line e e e || sndL (transpose t) == Line e e e || trdL (transpose t) == Line e e e ||
            fstE (fstL t) == e && sndE (sndL t) == e && trdE (trdL t) == e ||
            trdE (fstL t) == e && sndE (sndL t) == e && fstE (trdL t) == e

transpose :: Tic -> Tic
transpose (Tic (Line a1 b1 c1)
               (Line a2 b2 c2)
               (Line a3 b3 c3)) = Tic (Line a1 a2 a3)
                                      (Line b1 b2 b3)
                                      (Line c1 c2 c3)


main :: IO ()
main = do
    let f = Tic (Line O X X)
                (Line E O E)
                (Line X E O)
    print f
    print (hasWon f X)
    print (hasWon f O)
