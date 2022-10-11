module ClausesSpec
  ( rubric
  ) where

import Test.Hspec
import Test.HUnit
import Test.Hrubric

import Horn.Logic.Clauses (subst, subst_exp, Exp (..), Base (..))

rubric :: Rubric
rubric = do
  let num = Num 4
  let var = Var "subst"
  let unused = Var "unused"

  let e0 e = Plus [num, e, Var "x", e]
  let e1 e = Times [e0 e, num, e, Minus [Var "y"]]
  let e2 e = Minus [e0 e, Var "z", e1 e, Times [Var "x"]]

  passes "subst_exp" (1/2) $ do
    -- Var not present
    subst_exp num unused (e0 var) @?= e0 var
    subst_exp num unused (e1 var) @?= e1 var
    subst_exp num unused (e2 var) @?= e2 var

    -- Actual substitution
    subst_exp num var (e0 var) @?= e0 num
    subst_exp num var (e1 var) @?= e1 num
    subst_exp num var (e2 var) @?= e2 num
    subst_exp (e0 num) var (e2 var) @?= (e2 . e0) num
    subst_exp (e1 num) var (e2 var) @?= (e2 . e1) num

  let b0 e = And [Neg $ Geq (e1 e) e, Eq (e0 e) num]
  let b1 e = Or  [Neg $ Leq num (e2 e), Tr, And [b0 e], Or [b0 e]]
  let b2 e = Implies Fl $ Implies (b0 e) (b1 e)

  passes "subst" (1/2) $ do
    -- Var not present
    subst num unused (b0 $ e2 var) @?= (b0 . e2) var
    subst num unused (b1 $ e2 var) @?= (b1 . e2) var
    subst num unused (b2 $ e2 var) @?= (b2 . e2) var

    -- Actual substitution
    subst num var (b0 $ e2 var) @?= (b0 . e2) num
    subst num var (b1 $ e2 var) @?= (b1 . e2) num
    subst num var (b2 $ e2 var) @?= (b2 . e2) num
    subst (e1 num) var (b2 $ e2 var) @?= (b2 . e2 . e1) num
    subst (e2 num) var (b2 $ e2 var) @?= (b2 . e2 . e2) num
