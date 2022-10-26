module VCGenSpec
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
  [ "tests/pos/abs.js"
--, "tests/pos/driver.js"
  , "tests/pos/ifassert.js"
  , "tests/pos/inc.js"
  , "tests/pos/inc0.js"
--, "tests/pos/locks-loops.js"
  , "tests/pos/locks.js"
  , "tests/pos/max.js"
  , "tests/pos/skip.js"
  , "tests/pos/stmt.js"
--, "tests/pos/sum.js"
--, "tests/pos/sum2.js"
  , "tests/pos/test0.js"
  , "tests/pos/test1.js"
--, "tests/pos/twoloops.js"
--, "tests/pos/while-segments.js"
--, "tests/pos/while0.js"
  , "tests/pos/while5.js"
  ]

neg :: [String]
neg =
  [ "tests/neg/abs.js"
  , "tests/neg/assert.js"
  , "tests/neg/assume.js"
  , "tests/neg/inc.js"
  , "tests/neg/inc0.js"
  , "tests/neg/locks.js"
  , "tests/neg/max.js"
  , "tests/neg/stmt.js"
  , "tests/neg/test0.js"
  , "tests/neg/test1.js"
  , "tests/neg/while5-false.js"
  , "tests/neg/while5-init.js"
  , "tests/neg/while5-noninductive.js"
  , "tests/neg/while5-tooweak.js"
  ]

verify :: String -> IO Bool
verify f = do
  stmts <- Nano.parseNanoFromFile f
  let prog = Nano.SeqList stmts
  VCGen.checkVCs prog Logic.Tr Logic.Tr
  
rubric :: Rubric
rubric = do
  criterion "generateStmtVC" 1 . passOrFail $ do
    let check b f = it f $ verify f >>= (@?= b)
    let merge = foldM (\_ -> id) ()

    merge $ map (check True)  pos
    merge $ map (check False) neg
