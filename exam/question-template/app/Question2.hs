{-# LANGUAGE ImportQualifiedPost #-}
module Question2 where

import Control.Applicative
import Data.Set qualified as S

type Ident = String

data Expr
  = Var Ident
  | Lam Ident Expr
  | App Expr Expr
  deriving (Show)

{-----------------------------------------------------------------------------
                                     (a)
------------------------------------------------------------------------------}

free :: Expr -> S.Set Ident
free (Var x) = S.singleton x
free (App m n) = S.union (free m) (free n)
free (Lam x m) = free m S.\\ S.singleton x

{-----------------------------------------------------------------------------
                                     (b)
------------------------------------------------------------------------------}

fresh :: S.Set Ident -> Ident
fresh set = fresh' set "x"
  where 
    fresh' set candidate | candidate `S.member` set = fresh' set (candidate++"0")
                         | otherwise = candidate

{-----------------------------------------------------------------------------
                                     (c)
------------------------------------------------------------------------------}

subst :: Expr -> Ident -> Expr -> Expr
subst n x t@(Var y) | x == y = n 
                    | x /= y = t
subst n x t@(Lam y m) | x == y = t
                      | (x /= y) && not (y `S.member` free n) = Lam y (subst n x m)
                      | (x /= y) && (y `S.member` free n) = Lam y' (subst n x (subst (Var y') y m))
                        where 
                          y' = fresh (free m `S.union` free n)
subst n x (App m m') = App (subst n x m) (subst n x m') 

{-----------------------------------------------------------------------------
                                     (d)
------------------------------------------------------------------------------}
-- https://www.ietf.org/rfc/rfc2119.txt
-- should means I do not have to

tryBeta :: Expr -> Maybe Expr
tryBeta (Var _) = Nothing 
tryBeta (Lam x m) = case tryBeta m of
  Just m' -> pure (Lam x m')
  Nothing -> Nothing
tryBeta (App (Lam x m) n) = pure (subst n x m)
tryBeta (App m n) = case tryBeta m of
  Just m' -> pure (App m' n)
  Nothing -> case tryBeta n of
    Just n' -> pure (App m n')
    Nothing -> Nothing
