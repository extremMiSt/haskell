-- Copyright 2022 University of Freiburg
-- Janek Spaderna <janek.spaderna@pluto.uni-freiburg.de>
module MiniLang where

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
