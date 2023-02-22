-- Copyright 2023 University of Freiburg
-- Janek Spaderna <janek.spaderna@pluto.uni-freiburg.de>
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

module Parser
  ( -- This exports the operators (<*), (*>), (<*>), (<|>) and functions
    -- `empty`, `some`, `many`.
    module Control.Applicative,
    -- The parser type
    Parser,
    -- Parsers
    lit,
    satisfy,
    msatisfy,
    -- Running parsers. See their documentation for the differences.
    parse,
    parseLongest,
    exParser,
    ParseResult (..),
  )
where

import Control.Applicative

data ParseResult tok res
  = Match res
  | Partial res [tok]
  | Fail
  deriving (Show)

instance Functor (ParseResult tok) where
  fmap f = \case
    Match r -> Match (f r)
    Partial r ts -> Partial (f r) ts
    Fail -> Fail

instance Semigroup (ParseResult tok res) where
  -- Choose the leftmost match.
  Match r <> _ = Match r
  _ <> Match r = Match r
  -- If there is a partial match, choose the longer one (ie. the one with less
  -- tokens remaining).
  --
  -- Defined the way it is so that the partial lengths only have to be compared
  -- when there is no `Match` overall when doing a `fold` or `foldMap` by
  -- making use of lazyness.
  Partial r1 ts1 <> Partial r2 ts2 = Partial r ts
    where
      (r, ts) =
        if length ts1 <= length ts2
          then (r1, ts1)
          else (r2, ts2)
  -- Otherwise choose the branch which is not `Fail` (or `Fail` if both
  -- branches are `Fail`).
  Fail <> r = r
  r <> Fail = r

instance Monoid (ParseResult tok res) where
  mempty = Fail

-- | Returns the leftmost full match if there is any. If you want partial
-- matches see `parseLongest`.
parse :: Parser t r -> [t] -> Maybe r
parse p ts = case parseLongest p ts of
  Match r -> Just r
  _ -> Nothing

-- | Returns the leftmost result. In case of no full match the longest,
-- leftmost partial match is returned.
parseLongest :: Parser t r -> [t] -> ParseResult t r
parseLongest p = foldMap asResult . exParser p
  where
    asResult (r, []) = Match r
    asResult (r, ts) = Partial r ts

newtype Parser tok res = Parser ([tok] -> [(res, [tok])])

-- | Execute a parser. Returns the list of all (partial) parses.
exParser :: Parser tok res -> [tok] -> [(res, [tok])]
exParser (Parser p) = p

instance Functor (Parser tok) where
  fmap = pmap

instance Applicative (Parser tok) where
  pure = succeed
  (<*>) = pseq

instance Alternative (Parser tok) where
  empty = pempty
  (<|>) = palt

instance Monad (Parser tok) where
  pa >>= f = Parser \ts -> do
    (a, ts') <- exParser pa ts
    exParser (f a) ts'

-- | Recognizes the empty language.
pempty :: Parser t r
pempty = Parser (const [])

-- | Recognizes the empty string
succeed :: r -> Parser t r
succeed r = Parser \ts -> [(r, ts)]

-- | @satisfy p@ checks the first token using the predicate @p@.
satisfy :: (t -> Bool) -> Parser t t
satisfy p = Parser \case
  t : ts | p t -> [(t, ts)]
  _ -> []

-- | @msatisfy p@ checks and substitutes the first token using the predicate @p@.
msatisfy :: (t -> Maybe a) -> Parser t a
msatisfy p = Parser \case
  t : ts | Just a <- p t -> [(a, ts)]
  _ -> []

-- | Checks the literal appearance of a token.
lit :: Eq t => t -> Parser t t
lit t = satisfy (t ==)

-- | Alternative of two parsers.
palt :: Parser t r -> Parser t r -> Parser t r
palt p q = Parser \ts -> exParser p ts ++ exParser q ts

-- | Sequence two parsers.
pseq :: Parser t (s -> r) -> Parser t s -> Parser t r
pseq ps2r ps = Parser \ts ->
  [(s2r s, ts'') | (s2r, ts') <- exParser ps2r ts, (s, ts'') <- exParser ps ts']

-- | Map over a parser.
pmap :: (a -> b) -> Parser t a -> Parser t b
pmap f p = Parser \ts -> [(f a, ts') | (a, ts') <- exParser p ts]
