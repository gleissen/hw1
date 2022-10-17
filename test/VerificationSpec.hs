module VerificationSpec
  ( rubric
  ) where

import Test.Hspec
import Test.HUnit
import Test.Hrubric

import Control.Monad

import qualified Horn.Nano.Nano as Nano
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

verify :: String -> IO Bool
verify f = do
  stmts <- Nano.parseNanoFromFile f
  let prog = Nano.SeqList stmts
  VCGen.checkVCs prog Logic.Tr Logic.Tr

rubric :: Rubric
rubric = do
  criterion "invariants required" (3/4) . passOrFail $ do
    let check b f = it f $ verify f >>= (@?= b)
    let merge = foldM (\_ -> id) ()

    merge $ map (check True) pos
  passes "new cases" (1/4) $ do
    True @?= True
