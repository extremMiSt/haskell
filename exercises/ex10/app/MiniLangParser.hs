-- Copyright 2023 University of Freiburg
-- Janek Spaderna <janek.spaderna@pluto.uni-freiburg.de>
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Use lambda-case" #-}

module MiniLangParser where

import Control.Monad
import Data.Char qualified as C
import MiniLang
import Parser
import Data.Char (isDigit)

pstmts = pstmt `psepby1` lit (TSpecial ";")

pstmt  = pwhile <|> passign
  where
    pwhile = do 
      _ <- lit (TSpecial "while")
      e <- pexp
      _ <- lit (TSpecial "do")
      b <- pstmts
      _ <- lit (TSpecial "done")
      pure $ SWhile  e b
    passign = do
      v <- msatisfy \t -> case t of
        TId x -> Just x
        _ -> Nothing
      _ <- lit (TSpecial ":=")
      e <- pexp
      pure $ SAssign v e

pexp = pif <|> pcmp <|> pnot <|> paexp
  where
    pif = do
      _ <- lit (TSpecial "if")
      e <- pexp
      _ <- lit (TSpecial "then")
      b <- pexp
      _ <- lit (TSpecial "else")
      c <- pexp
      pure $ EIf e b c
    pcmp = do 
      x <- paexp
      pure [x]
      --c - msatisfy \t -> case t of

      

-------------------------------------------------------------------------------
-- Lexer

data Token
  = -- | A variable identifier (not a keyword!)
    --
    -- > [a-zA-Z][a-zA-Z0-9]*
    TId Var
  | -- | An integer.
    --
    -- > [0-9]+
    TNum Integer
  | -- | A symbol or keyword. For example
    --
    -- > TSym ";"
    -- > TSym ":="
    -- > TSym "while"
    TSpecial String
  deriving (Eq, Show)

-- | Tokenizes a string.
lexer :: String -> Maybe [Token]
lexer = parse $ many $ skipSpaces *> ptoken <* skipSpaces

-- | Parses a single token.
ptoken :: Parser Char Token
ptoken = pident <|> psym
  where 
    keywords = ["if", "then", "else", "fi", "while", "do", "done", "not"]
    pident = do
      x <- satisfy (\c -> C.isAsciiLower c || C.isAsciiUpper c)
      xs <- many $ satisfy (\c -> C.isAsciiLower c || C.isAsciiUpper c || C.isDigit c)
      if x:xs `elem` keywords
        then pure $ TSpecial (x:xs)
        else pure $ TId (x:xs)
    psym = TSpecial <$> asum [tokens ";", tokens ":=", 
                              tokens "+", tokens "-", 
                              tokens "*", tokens "/",
                              tokens "<", tokens "=",
                              tokens "(", tokens ")"] 
    

-------------------------------------------------------------------------------
-- Helpers

-- | Parse a list of literal tokens.
tokens :: Eq a => [a] -> Parser a [a]
tokens = mapM lit

-- | Parses an unsigned integer.
pinteger :: Parser Char Integer
pinteger = read <$> some (satisfy C.isDigit)

-- | A parser to skip over zero or more spaces.
skipSpaces :: Parser Char ()
skipSpaces = void $ many (satisfy C.isSpace)

psepby :: Parser t r -> Parser t sep -> Parser t [r]
psepby p s = psepby1 p s <|> pure []

psepby1 :: Parser t r -> Parser t sep -> Parser t [r]
psepby1 p s = do
    x <- p
    xs <- many do
        _ <- s
        p
    pure (x:xs)