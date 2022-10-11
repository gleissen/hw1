module NanoSpec
  ( rubric
  ) where

import Test.Hspec
import Test.HUnit
import Test.Hrubric

rubric :: Rubric
rubric = do
  criterion "ECMAscript" (2/3) $ do
    passes "stub" 1 $ do
      True @?= True
  criterion "Logic" (1/3) $ do
    passes "stub" 1 $ do
      True @?= True
