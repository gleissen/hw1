import Test.Hspec
import Test.Hrubric
import Test.HUnit

import System.Environment
import System.Console.ANSI
import Text.Printf

import Control.Monad (when)
import Data.Maybe (isJust)

import qualified ClausesSpec
import qualified NanoSpec
import qualified VCGenSpec

rubric :: Rubric
rubric = do
  criterion "Code" (6/10) $ do
    criterion "Clauses" (1/6) ClausesSpec.rubric
    criterion "Nano"    (3/6) NanoSpec.rubric
    criterion "VCGen"   (2/6) VCGenSpec.rubric
  criterion "Verification" (4/10) $ do
    criterion "Invariant" (3/4) (passes "stub" 1 $ True @?= True)
    criterion "New"       (1/4) (passes "stub" 1 $ True @?= True)
--    criterion "Invariant" (3/4) InvariantSpec.rubric
--    criterion "New"       (1/4) NewSpec.rubric

-- Output the weight as grade
output :: Float -> IO ()
output g = do
  let adj = min 1 (0.1 + g * 0.9)
  let color = if adj > 0.55 then Green else Red
  setSGR [SetConsoleIntensity BoldIntensity]
  putStr "Your current grade is: ["
  setSGR [SetColor Foreground Vivid color]
  putStr $ printf "%.1f" (adj * 10)
  setSGR [Reset, SetConsoleIntensity BoldIntensity]
  putStr $ "/10.0]\n"
  setSGR [Reset]

  -- Output the weight as a grade between 0 and 1 for codegrade
  codegrade <- lookupEnv "CG_INFO"
  when (isJust codegrade) $ print adj

main :: IO ()
main = do
  result <- hrubric rubric
  case result of
    Left p -> putStrLn $ "Error in rubric nesting: '" ++ p ++ "'"
    Right g -> output g
