module VerificationSpec
  ( rubric
  ) where

import System.Environment

import Test.Hspec
import Test.HUnit
import Test.Hrubric

import Control.Monad

import Horn.Nano.Nano
import qualified Horn.Logic.Clauses as Logic
import qualified Horn.VCGen.VCGen as VCGen

pos :: [String]
pos =
  [ "tests/pos/locks-loops.js"
  , "tests/pos/sum.js"
  , "tests/pos/sum2.js"
  , "tests/pos/twoloops.js"
  , "tests/pos/while-segments.js"
  , "tests/pos/while0.js"
  ]

new :: [String]
new =
  [ "tests/pos-new/case0.js"
  , "tests/pos-new/case1.js"
  , "tests/pos-new/case2.js"
  ]

-- Normalize and remove invariants
normalize :: Stmt -> Stmt
normalize (SeqList ss)  = case normList ss of
  []  -> Skip
  [s] -> s
  ss' -> SeqList ss'
normalize (Seq s1 s2)   = normalize $ SeqList [s1, s2]
normalize (If p s1 s2)  = If p (normalize s1) (normalize s2)
normalize (While _ p s) = While (Logic.And []) p (normalize s)
normalize s             = s

normList :: [Stmt] -> [Stmt]
normList = unskip . map normalize >=> flatten

flatten :: Stmt -> [Stmt]
flatten (SeqList ss) = ss
flatten s            = [s]

unskip :: [Stmt] -> [Stmt]
unskip = foldr unskip' []
  where
    unskip' Skip acc = acc
    unskip' s    acc = s:acc

withInv :: String -> IO Bool
withInv f = do
  stmts <- parseNanoFromFile f
  let prog = SeqList stmts

  fixtures <- lookupEnv "FIXTURES"
  case fixtures of
    Nothing -> VCGen.checkVCs prog Logic.Tr Logic.Tr
    Just p  -> do
      stmts' <- parseNanoFromFile $ p ++ f
      let prog' = SeqList stmts'
      -- if equivalent modulo normalisation
      if normalize prog == normalize prog'
        then VCGen.checkVCs prog Logic.Tr Logic.Tr
        else return False

nonTrivial :: String -> IO Bool
nonTrivial f = do
  stmts <- parseNanoFromFile f
  let prog = SeqList stmts
  sat <- VCGen.checkVCs prog Logic.Tr Logic.Tr
  sat' <- VCGen.checkVCs (normalize prog) Logic.Tr Logic.Tr
  return $ sat == True && sat' == False

rubric :: Rubric
rubric = do
  let merge = foldM (\_ -> id) ()

  criterion "invariants required" (3/4) . distribute $ do
    let check f = passes f (0/0) $ withInv f >>= (@?= True)
    merge . map check $ pos
  criterion "new cases" (1/4) . distribute $ do
    let check f = passes f (0/0) $ nonTrivial f >>= (@?= True)
    merge . map check $ new
