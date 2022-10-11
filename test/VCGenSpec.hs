module VCGenSpec
  ( rubric
  ) where

import Test.Hspec
import Test.HUnit
import Test.Hrubric

rubric :: Rubric
rubric = do
  passes "stub" 1 $ do
    True @?= True
