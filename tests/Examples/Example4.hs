{-# LANGUAGE BlockArguments #-}
module Examples.Example4 where

import Test.Tasty
import Test.Tasty.HUnit
import Netting.Sem
import Netting.AmmFuns

import Data.Map
import Data.Foldable
import qualified Data.Sequence as DS

example4 :: TestTree
example4 =
  testCaseInfo "example 4 \"Netting worse than standard\"\n" do
    let ex4_amms =
          [(AMM (T0, 12) (T1, 12)), 
            (AMM (T1, 18) (T2,  8))]
        ex4_txns =
          [ Transaction "A" (T1, 6) (T0, 4),
            Transaction "A" (T2, 4) (T1, 6),
            Transaction "B" (T1, 6) (T0, 4) ]
        ex4_q_len      = 2
        ex4_a          = User (fromList [(T0, 0), (T1, 0), (T2, 4)]) "A"
        ex4_b          = User (fromList [(T0, 4), (T1, 6), (T2, 0)]) "B"
        ex4_init_state = (ex4_amms, [ex4_a, ex4_b])
        ex4_init_conf  = Configuration ex4_init_state ex4_init_state DS.Empty
        (res, log)     = runTransactions False ex4_init_conf ex4_txns ex4_q_len

        expected = Configuration 
          -- green
          ([AMM (T0,  8.0) (T1, 18.0),
            AMM (T2, 12.0) (T1, 12.0) ],
            [User (fromList [(T0, 4.0),(T1, 0.0),(T2, 0.0)]) "A",
            User (fromList [(T0, 4.0),(T1, 6.0),(T2, 0.0)]) "B"]) 
          -- simulated
          ([AMM (T0,  8.0) (T1, 18.0),
            AMM (T2, 12.0) (T1, 12.0) ],
            [User (fromList [(T0, 4.0),(T1, 0.0),(T2, 0.0)]) "A",
            User (fromList [(T0, 4.0),(T1, 6.0),(T2, 0.0)]) "B"]) 
          -- queue
          (DS.Empty)
        err_message = unlines (log ++ ["expected conf:"] ++ [(show expected)] ++ ["but got:"] ++ [(show res)])

    assertBool err_message (res == expected)
    return $ unlines log