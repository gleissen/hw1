 {-# LANGUAGE OverloadedStrings #-}
module Horn.VCGen.VCGen where

import           Control.Monad.State.Strict
import qualified Horn.Nano.Nano as Nano
import qualified Horn.Logic.Clauses as Logic
import qualified Horn.Bindings.Z3 as Z3
import           Debug.Trace
import           Control.Monad.Trans.Class  (lift)
import           Data.Foldable (foldrM)
import           Rainbow
import Data.Function ((&))

data VCState = VCS { vc :: [Logic.Base]}
type VCM = StateT VCState IO

-----------------------------------------------------------------------------------
generateStmtVC :: Nano.Stmt -> Logic.Base -> VCM Logic.Base 
-----------------------------------------------------------------------------------
generateStmtVC (Nano.Assume phi) post = do
              let pre = Logic.Implies phi post
              return pre

generateStmtVC (Nano.Assert phi) post = do
              let pre = Logic.And [post, phi]
              return pre              

generateStmtVC _ _ = error "TODO: FILL THIS IN"

-------------------------------------------------------------------
isValid :: Logic.Base -> IO Bool
-------------------------------------------------------------------
isValid pre = do
         b <- Z3.implies (Logic.Tr) pre
         return b

-------------------------------------------------------------------
checkVCs :: Nano.Stmt -> Logic.Base -> Logic.Base -> IO Bool
-------------------------------------------------------------------
checkVCs pgm post init = do 
        res <- runStateT (generateStmtVC pgm post) initState
        let pre = fst $ res
        let vcs = vc $ snd $ res
        sol1 <- Z3.implies init pre
        sol2 <- isValid (Logic.And vcs)
        return $ sol1 && sol2

-------------------------------------------------------------------
initState ::  VCState
-------------------------------------------------------------------
initState = VCS []

-------------------------------------------------------------------
getVCs :: VCM [Logic.Base]
-------------------------------------------------------------------
getVCs = vc <$> get

-------------------------------------------------------------------
addVC :: Logic.Base -> VCM ()
-------------------------------------------------------------------
addVC b = do
              st <- get
              let vcs = b:(vc st)
              put VCS {vc = vcs}

-------------------------------------------
verifyFile :: FilePath ->  IO (Bool)
-------------------------------------------
verifyFile f = do
        stmts <- Nano.parseNanoFromFile f
        let prog = Nano.SeqList stmts
        putStr $ "Checking the file : " ++ (show f) ++ "\n"-- ++ (show stmts)
        res <- checkVCs prog Logic.Tr Logic.Tr 
        printResult res
        return res

-------------------------------------------
printResult :: Bool -> IO()
-------------------------------------------
printResult True = do
        putStr $ "Verification: "
        putChunkLn $ "passed"   & fore green

printResult False = do
        putStr $ "Verification: "
        putChunkLn $ "failed"  & fore red

-------------------------------------------
test :: IO ()
-------------------------------------------
test = do
    res <- verifyFile "tests/pos/max.js"    
    return ()
    
    --return ()
    --where

      --a2 = Nano.Assign "x"  (Nano.Plus (Nano.Var "y") (Nano.Num 1))
      --a1 = Nano.Assign "y"  (Nano.Var "z")
      --pgm = Nano.SeqList [a1,a2]
      --post = Logic.Geq (Logic.Var "x") (Logic.Num 1)
      -- {True} if y<=0 then x:=1 else x:=y {x > 0}
      -- cond = Nano.Lte (Nano.Var "y") (Nano.Num 0)
      -- s1 = Nano.Assign "x" (Nano.Num 1)
      -- s2 = Nano.Assign "x" (Nano.Var "y")
      -- pgm = Nano.If cond s1 s2
      -- post = Logic.Geq (Logic.Var "x") (Logic.Num 0) 
      ---{x=0} while (I:=x=<6) x=<5 x=x+1 {x=6}
      --cond = Nano.Lte (Nano.Var "x") (Nano.Num 5)
      --s = Nano.Assign "x" (Nano.Plus (Nano.Var "x") (Nano.Num 1))
      --inv = Logic.Leq (Logic.Var "x") (Logic.Num 6)
      --pgm = Nano.While inv cond s 
      --post = Logic.Eq (Logic.Var "x") (Logic.Num 6) 
      --init = (Logic.Eq (Logic.Var "x") (Logic.Num 0))
