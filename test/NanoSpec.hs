module NanoSpec
  ( rubric
  ) where

import Test.Hspec
import Test.HUnit
import Test.Hrubric

import Control.Monad ((>=>), (<=<))

import Horn.Nano.Nano
import qualified Horn.Logic.Clauses as L
import Language.ECMAScript3.Parser (parse, expression, statement, program)
import Language.ECMAScript3 (Statement, unJavaScript)

rubric :: Rubric
rubric = do
  criterion "Logic" (1/3) $ do
    let e0 e = (e `Minus` Num 0) `Times` Var "x"
    let e1 e = (e0 e `Minus` Var "y") `Plus` (e `Plus` Num 2)
    let e2 e = e1 e `Times` Var "y" `Minus` e

    let e0' e = L.Times [L.Minus [e, L.Num 0], L.Var "x"]
    let e1' e = L.Plus [L.Minus [e0' e, L.Var "y"], L.Plus [e, L.Num 2]]
    let e2' e = L.Minus [L.Times [e1' e, L.Var "y"], e]

    passes "expToBase" (1/2) $ do
      expToBase (e0 $ Var "x") @?= e0' (L.Var "x")
      expToBase (e1 $ Var "y") @?= e1' (L.Var "y")
      expToBase (e2 $ Var "z") @?= e2' (L.Var "z")

      expToBase (e2 . e0 $ Var "a") @?= (e2' . e0' $ L.Var "a")
      expToBase (e2 . e1 $ Var "b") @?= (e2' . e1' $ L.Var "b")
      expToBase (e2 . e2 $ Var "c") @?= (e2' . e2' $ L.Var "c")

    let b0 e = (e `Lte` e0 e) `And` (e1 e `Gte` e)
    let b1 e = (e2 e `Neq` e) `Or` (e2 e `Eq` e2 e) `And` b0 e
    let b2 e = b1 e `And` (b0 e `Or` (e2 e `Neq` e1 e))

    let b0' e = L.And [e `L.Leq` e0' e, e1' e `L.Geq` e]
    let b1' e = L.And [L.Or [L.Neg $ e2' e `L.Eq` e, e2' e `L.Eq` e2' e], b0' e]
    let b2' e = L.And [b1' e, L.Or [b0' e, L.Neg $ e2' e `L.Eq` e1' e]]

    passes "bexpToBase" (1/2) $ do
      bexpToBase (b0 $ Var "x") @?= b0' (L.Var "x")
      bexpToBase (b1 $ Var "y") @?= b1' (L.Var "y")
      bexpToBase (b2 $ Var "z") @?= b2' (L.Var "z")

  criterion "ECMAscript" (2/3) $ do
    let unwrap (Right x) = x
        unwrap (Left  x) = error . show $ x

    -- Nano parsers
    let expr = toNanoExp  . unwrap . parse expression ""
    let bool = toNanoBexp . unwrap . parse expression ""

    -- Expressions
    let e0 = "x + 4 * 23 - y"
    let e1 = "x * (y + 15 * 23) * 3"
    let e2 = "x - y - z - 5 * 2"
    let e3 = "x + (y * 25) + 4 + z"

    -- In syntax tree format
    let e0' = Minus (Plus (Var "x") (Times (Num 4) (Num 23))) (Var "y")
    let e1' = Times (Times (Var "x") (Plus (Var "y") (Times (Num 15) (Num 23)))) (Num 3)
    let e2' = Minus (Minus (Minus (Var "x") (Var "y")) (Var "z")) (Times (Num 5) (Num 2))
    let e3' = Plus (Plus (Plus (Var "x") (Times (Var "y") (Num 25))) (Num 4)) (Var "z")

    passes "toNanoExp" (1/4) $ do
      expr e0 @?= e0'
      expr e1 @?= e1'
      expr e2 @?= e2'
      expr e3 @?= e3'

    -- Boolean Expressions
    let b0 = concat [e0, ">=", e1, "&& true"]
    let b1 = concat [e3, "!=", e2, "||", b0]
    let b2 = concat ["false && (", b1, ") &&", e0, "<=", e3]
    let b3 = concat [e2, "==", e1, "||", b2, "||", "true"]

    -- In syntax tree format
    let b0' = (e0' `Gte` e1') `And` (Bool True)
    let b1' = (e3' `Neq` e2') `Or` b0'
    let b2' = ((Bool False) `And` b1') `And` (e0' `Lte` e3')
    let b3' = ((e2' `Eq` e1') `Or` b2') `Or` (Bool True)
 
    passes "toNanoBexp" (1/4) $ do
      bool b0 @?= b0'
      bool b1 @?= b1'
      bool b2 @?= b2'
      bool b3 @?= b3'

    passes "toNanoStmt" (2/4) $ do
      let flatten :: Stmt -> [Stmt]
          flatten (SeqList ss) = ss
          flatten s            = [s]

      let unskip :: [Stmt] -> [Stmt]
          unskip = foldr unskip' []
            where
              unskip' Skip acc = acc
              unskip' s    acc = s:acc

      -- We remove invariants so students can
      -- add them to the test cases without these
      -- tests failing
      let normalize :: Stmt -> Stmt
          normalize (SeqList ss)  = case normList ss of
                                      []  -> Skip
                                      [s] -> s
                                      ss' -> SeqList ss'
          normalize (Seq s1 s2)   = normalize $ SeqList [s1, s2]
          normalize (If p s1 s2)  = If p (normalize s1) (normalize s2)
          normalize (While _ p s) = While (L.And []) p (normalize s)
          normalize s             = s

          normList :: [Stmt] -> [Stmt]
          normList = unskip . map normalize >=> flatten

      let parseNorm :: String -> IO [Stmt]
          parseNorm = parseNanoFromFile >=> return . normList

      -- Some positive test files
      parseNorm "tests/pos/abs.js" `shouldReturn`
        [ Assign "res" (Num 0)
        , If (Gte (Var "x") (Num 1))
             (Assign "res" (Var "x"))
             (Assign "res" (Minus (Num 0) (Var "x")))
        , Assert $ L.Var "res" `L.Geq` L.Num 0
        ]

      parseNorm "tests/pos/driver.js" `shouldReturn`
        [ Assume . L.Neg $ L.Var "newCount" `L.Eq` L.Var "oldCount"
        , Assign "lock" (Num 0)
        , While (L.And [])
                (Neq (Var "newCount") (Var "oldCount"))
                (SeqList
                  [ Assign "lock" (Num 1)
                  , Assign "oldCount" (Var "newCount")
                  , If (Gte (Var "newCount") (Num 1))
                       (SeqList
                         [ Assign "lock" (Num 0)
                         , Assign "newCount" (Minus (Var "newCount") (Num 1))
                         ])
                       Skip
                  ])
        , Assert . L.Neg $ L.Var "lock" `L.Eq` L.Num 0
        ]

      parseNorm "tests/pos/while5.js" `shouldReturn`
        [ Assign "x" (Num 0)
        , While (L.And [])
                (Lte (Var "x") (Num 5))
                (Assign "x" (Plus (Var "x") (Num 1)))
        , Assert $ L.Var "x" `L.Eq` L.Num 6
        ]

      parseNorm "tests/pos/locks-loops.js" `shouldReturn`
        [ Assign "lock" (Num 0)
        , While (L.And [])
                (Gte (Var "n") (Num 1))
                (SeqList
                  [ If (Gte (Var "flag") (Num 1))
                       (Assign "lock" (Num 1))
                       Skip
                  , If (Gte (Var "flag") (Num 1))
                       (Assign "lock" (Num 0))
                       Skip
                  , If (Gte (Var "flag") (Num 1))
                       (Assign "lock" (Num 1))
                       Skip
                  , If (Gte (Var "flag") (Num 1))
                       (Assign "lock" (Num 0))
                       Skip
                  , Assign "n" (Minus (Var "n") (Num 1))
                  ])
        , Assert $ L.Var "lock" `L.Eq` L.Num 0
        ]

      parseNorm "tests/pos/twoloops.js" `shouldReturn`
        [ Assign "i" (Num 0)
        , While (L.And [])
                (Lte (Var "i") (Num 3))
                (Assign "i" (Plus (Var "i") (Num 1)))
        , While (L.And [])
                (Lte (Var "i") (Num 7))
                (Assign "i" (Plus (Var "i") (Num 1)))
        , Assert $ L.Var "i" `L.Eq` L.Num 8
        ]

      parseNorm "tests/pos/ifassert.js" `shouldReturn`
        [ Assign "lock" (Num 0)
        , If (Gte (Var "x") (Num 1))
             (SeqList
               [ Assign "lock" (Num 1)
               , Assert (L.And [L.Var "lock" `L.Geq` L.Num 1, L.Var "x" `L.Geq` L.Num 1])
               ])
             Skip
        , If (Gte (Var "y") (Num 1))
             (SeqList
               [ Assign "lock" (Num 0)
               , Assert (L.And [L.Var "lock" `L.Leq` L.Num 0, L.Var "y" `L.Geq` L.Num 1])
               ])
             Skip
        ]

      parseNorm "tests/pos/sum2.js" `shouldReturn`
        [ Assign "i" (Num 1)
        , Assign "sum" (Num 0)
        , While (L.And [])
                (Gte (Var "n") (Var "i"))
                (SeqList
                  [ Assign "j" (Num 1)
                  , While (L.And [])
                          (Gte (Var "i") (Var "j"))
                          (SeqList
                            [ Assign "sum" (Plus (Var "sum") (Var "j"))
                            , Assign "j" (Plus (Var "j") (Num 1))
                            , Assign "i" (Plus (Var "i") (Num 1))
                            ])
                  , Assert $ L.Var "sum" `L.Geq` L.Num 0
                  ])
        ]

      -- Some negative test files
      parseNorm "tests/neg/abs.js" `shouldReturn`
        [ Assign "res" (Num 0)
        , If (Gte (Var "x") (Num 1))
             (Assign "res" (Var "x"))
             (Assign "res" (Minus (Num 0) (Var "x")))
        , Assert $ L.Var "res" `L.Geq` L.Num 1
        ]

      parseNorm "tests/neg/while5-false.js" `shouldReturn`
        [ Assign "x" (Num 0)
        , While (L.And [])
                (Lte (Var "x") (Num 5))
                (Assign "x" (Plus (Var "x") (Num 1)))
        , Assert $ L.Var "x" `L.Eq` L.Num 5
        ]
      parseNorm "tests/neg/while5-noninductive.js" `shouldReturn`
        [ Assign "x" (Num 0)
        , While (L.And [])
                (Lte (Var "x") (Num 5))
                (Assign "x" (Plus (Var "x") (Num 1)))
        ]

      -- This last test checks whether
      -- invariants are passed to the while
      let normWhile (While i b s) = While i b (normalize s)
          normWhile s             = s

      let invParse = return . map normWhile <=< mkNano . unJavaScript . unwrap . parse program ""

      invParse "function test(x) { while (true) { invariant(x == 1) } }" @?= Just
        [ While (L.And [L.Var "x" `L.Eq` L.Num 1])
                (Bool True)
                Skip
        ]
