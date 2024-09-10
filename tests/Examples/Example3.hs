{-# LANGUAGE BlockArguments #-}
module Examples.Example3 where

import Test.Tasty
import Test.Tasty.HUnit
import Netting.Sem
import Netting.AmmFuns

import Data.Map
import Data.Foldable
import qualified Data.Sequence as DS

example3 :: TestTree
example3 = 
  testCaseInfo "example 3 \"Simultaneous Exchange\"\n" do
    let ex3_amms =
          [(AMM (T0, 12) (T1, 12)), 
            (AMM (T1, 12) (T2, 12))]
        ex3_txns =
          [ Transaction "B" (T1, 6) (T2, 4),
            Transaction "A" (T1, 6) (T0, 4),
            Transaction "A" (T2, 4) (T1, 6),
            Transaction "B" (T0, 4) (T1, 6) ]
        ex3_q_len      = 3
        ex3_a          = User (fromList [(T0, 0), (T1, 0), (T2, 4)]) "A"
        ex3_b          = User (fromList [(T0, 4), (T1, 0), (T2, 0)]) "B"
        ex3_init_state = (ex3_amms, [ex3_a, ex3_b])
        ex3_init_conf  = Configuration ex3_init_state ex3_init_state DS.Empty
        (res, log)     = runTransactions False ex3_init_conf ex3_txns ex3_q_len

        expected = Configuration 
          -- green
          ([AMM (T0, 12.0) (T1, 12.0),
            AMM (T2, 12.0) (T1, 12.0) ],
            [User (fromList [(T0, 4.0),(T1, 0.0),(T2, 0.0)]) "A",
            User (fromList [(T0, 0.0),(T1, 0.0),(T2, 4.0)]) "B"]) 
          -- simulated
          ([AMM (T0, 12.0) (T1, 12.0),
            AMM (T2, 12.0) (T1, 12.0)],
            [User (fromList [(T0, 4.0),(T1, 0.0),(T2, 0.0)]) "A",
            User (fromList [(T0, 0.0),(T1, 0.0),(T2, 4.0)]) "B"])
          -- queue
          (DS.Empty)
        err_message = unlines (log ++ ["expected conf:"] ++ [(show expected)] ++ ["but got:"] ++ [(show res)])

    assertBool err_message (res == expected)
    return $ unlines log