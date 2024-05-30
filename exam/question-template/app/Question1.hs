{-# LANGUAGE ImportQualifiedPost #-}
module Question1 where

import Data.Map qualified as M
import Parser
import Data.Char (ord)
import Data.Bits (Bits(xor))

type Ident = Char

data Term = Term {symbol :: Ident, subterms :: [Term]}
  deriving (Eq, Show)

type Signature = M.Map Ident Int

{-----------------------------------------------------------------------------
                                     (a)
------------------------------------------------------------------------------}

parseTerm :: Parser Char Term
parseTerm = do
  sym <- parseSymbol
  _ <- lit '('
  args <- parseA
  _ <- lit ')'
  pure $ Term sym args

parseA :: Parser Char [Term]
parseA = eps <|> tb
  where 
    eps = pure []
    tb = do
      t <- parseTerm
      b <- parseB
      pure $ t:b 

parseB :: Parser Char [Term]
parseB = eps <|> ktb
  where 
    eps = pure []
    ktb = do
      _ <- lit ','
      t <- parseTerm
      b <- parseB
      pure $ t:b

parseSymbol :: Parser Char Char
parseSymbol = satisfy (\x -> (ord x >= 65 && ord x <= 90) || (ord x >= 97 && ord x <= 122))

{-----------------------------------------------------------------------------
                                     (b)
------------------------------------------------------------------------------}

type Path = [Int]

arityCheck :: Signature -> Term -> Either Path Term
arityCheck sig t = case arityCheck2 sig t [] of
  Left x -> Left $ reverse x --look ugly, I could use '++[n]' in line 63, but this seems more efficient.
  Right a -> return a
  where 
  arityCheck2 sig t@(Term sym sub) path = if length sub == (sig M.! sym)
    then
      case sequence [arityCheck2 sig tn (n:path)| (tn, n) <- zip sub [0..]] of --lets hope this single usage of "sequence", plus the returns qualifies as using the monad
        Left x -> Left x
        Right a -> return t
    else 
      Left path