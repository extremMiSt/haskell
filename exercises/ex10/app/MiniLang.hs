-- Copyright 2022 University of Freiburg
-- Janek Spaderna <janek.spaderna@pluto.uni-freiburg.de>
{-# LANGUAGE QualifiedDo #-}

module MiniLang where

import qualified Data.Map.Strict as Map
import Data.Maybe
import SimplePrelude as SP
import Prelude ()

-------------------------------------------------------------------------------
-- Program Data Types

type Var = String

type Prog = [Stmt]

data Stmt
  = -- | > SWhile cond body   ~ while cond do body done
    SWhile Expr [Stmt]
  | -- | > SAssign var expr   ~ var := expr
    SAssign Var Expr

data Expr
  = -- | > EVar var           ~ var
    EVar Var
  | -- | > ENum num           ~ num
    ENum Integer
  | -- | > ENot expr          ~ ! expr
    ENot Expr
  | -- | > EOp lhs op rhs     ~ lhs OP rhs
    EOp Expr BinOp Expr
  | -- | > EIf cond e₁ e₂     ~ if cond then e₁ else e₂ end
    EIf Expr Expr Expr

data BinOp
  = Equal
  | Less
  | Add
  | Sub
  | Mul
  | Div

-------------------------------------------------------------------------------
-- State monad

-- The `runState` function has type
--    runState :: State s a -> s -> (a, s)
newtype State s a = State {runState :: s -> (a, s)}

instance Monad (State s) where
  return a = State $ \s ->
    -- We return the value `a` without modifying the state `s`.
    (a, s)
  m >>= f = State $ \s ->
    let -- Execute the first action `m` with the initial state.
        (a, s') = runState m s
        -- Evaluate `f` to get the next action `n`.
        n = f a
        -- Execute the action `n` using the updated state from `m`.
        (b, s'') = runState n s'
     in -- Return the result value and the resulting state.
        (b, s'')

-------------------------------------------------------------------------------
-- Domain specific state monad functions

getVar :: Var -> State Memory Integer
getVar v = State $ \s ->
  ( -- Get the value from the map or return 0.
    0 `fromMaybe` Map.lookup v s,
    -- We don't modify the state.
    s
  )

setVar :: Var -> Integer -> State Memory ()
setVar v x = State $ \s ->
  ( -- Nothing interesting to return here.
    (),
    -- Update the state to include the new variable.
    Map.insert v x s
  )

-------------------------------------------------------------------------------
-- Evaluation functions

type Memory = Map.Map Var Integer

evalExpr :: Expr -> State Memory Integer
-- Variables evaluate to the stored value.
evalExpr (EVar v) = getVar v
-- Numbers evaluate to themselves.
evalExpr (ENum n) = return n
-- Evaluate the child expression and negate the result.
evalExpr (ENot ex) = SP.do
  x <- evalExpr ex
  return $ boolToNum $ not $ numToBool x
-- Evaluate LHS and RHS and feed them to the operator.
evalExpr (EOp lhs o rhs) = SP.do
  a <- evalExpr lhs
  b <- evalExpr rhs
  return $ op o a b
  where
    op :: BinOp -> Integer -> Integer -> Integer
    op Add = (+)
    op Sub = (-)
    op Mul = (*)
    op Div = div
    op Equal = \x y -> boolToNum $ x == y
    op Less = \x y -> boolToNum $ x < y
-- Evaluate the condition. If it is `True` evaluate and return the value of
-- `trueB`; otherwise, evaluate and return the value of `falseB`.
evalExpr (EIf ex trueB falseB) = SP.do
  x <- evalExpr ex
  if numToBool x
    then evalExpr trueB
    else evalExpr falseB

evalStmt :: Stmt -> State Memory ()
-- Evaluate the condition. If it is `True` evaluate the child statements and
-- recurse.
evalStmt s@(SWhile ex sts) = SP.do
  x <- evalExpr ex
  if numToBool x
    then SP.do
      evalProg sts
      evalStmt s
    else return ()
-- Evaluate the expression and store the value in the variable.
evalStmt (SAssign v ex) = SP.do
  x <- evalExpr ex
  setVar v x

-- | Sequences the evaluation of a list of statements.
evalProg :: [Stmt] -> State Memory ()
evalProg = foldr (>>) (return ()) . map evalStmt

-- | Returns `True` iff the input is not zero.
numToBool :: Integer -> Bool
numToBool = (/= 0)

-- | Turns a boolean into a number. The result is either `1` or `0`.
boolToNum :: Bool -> Integer
boolToNum True = 1
boolToNum False = 0

-- | Evaluates a program's statements starting from the empty `Memory`.
runProg :: Prog -> Memory
runProg p = case evalProg p of
  State f -> snd $ f Map.empty

-------------------------------------------------------------------------------
-- Example program

-- | Sums the numbers in the given range into `sum`.
--
-- Assumes that `Add` can be used in place of a boolean OR.
rangeSum :: Integer -> Integer -> Prog
rangeSum a b =
  [ SAssign "a" (ENum a),
    SAssign "b" (ENum b),
    SWhile
      ( EOp
          (EOp (EVar "a") Less (EVar "b"))
          Add
          (EOp (EVar "a") Equal (EVar "b"))
      )
      [ SAssign "sum" (EOp (EVar "sum") Add (EVar "a")),
        SAssign "a" (EOp (EVar "a") Add (ENum 1))
      ]
  ]