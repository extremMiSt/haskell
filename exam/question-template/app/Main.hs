module Main where
import Question1
import Parser
import qualified Data.Map as M
import Data.Maybe
import Question2

main :: IO ()
main = do
    putStrLn "question 1"
    print $ parse parseTerm "a()"
    print $ parse parseTerm "a(b(),c(),d(a(),a()))"
    print $ parse parseTerm "a(b(),c(),d(a(),a())"
    print $ parse parseTerm "0(b(),c(),d(a(),a()))"

    let sig = M.fromList [('x', 0)]
    print $ arityCheck sig (Term 'x' [])
    print $ arityCheck sig (Term 'x' [Term 'x' []])

    let sig2 = M.fromList [('x', 0), ('y', 2)]
    print $ arityCheck sig2 $ fromJust $ parse parseTerm "y(y(x(),x(x())),x())"

-------------------------------------------------------------------------------
    putStrLn ""
    putStrLn "question 2"

    let ex0 = Lam "y" (App (Var "x") (Var "y"))
    let ex0' = Var "y"

    print $ free ex0
    print $ free ex0'

    print $ fresh $ free ex0
    print $ fresh $ free ex0'

    print $ subst ex0' "x" ex0

    let ex1 = App (Lam "x" (Var "x")) (Var "y")
    let ex2 = App (Var "x") ex1
    let ex3 = Lam "x" ex2
    print $ tryBeta ex1
    print $ tryBeta ex2
    print $ tryBeta ex3

