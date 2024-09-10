{-# LANGUAGE BlockArguments #-}
module Examples.Example5 where

import Test.Tasty
import Test.Tasty.HUnit
import Netting.Sem
import Netting.AmmFuns

import Data.Map
import Data.Foldable
import qualified Data.Sequence as DS

example5 :: TestTree
example5 =
  testCaseInfo "example 5 \"Netting scenario\"\n" do
    let ex5_amms =
          [(AMM (T0, 12) (T1, 12)), 
            (AMM (T1, 18) (T2,  8)),
            (AMM (T2, 12) (T0, 12))]
        ex5_txns =
          [ Transaction "A" (T2, 6) (T0, 4),
            Transaction "A" (T1, 6) (T0, 4),
            Transaction "A" (T2, 4) (T1, 6) ]
        ex5_q_len      = 2
        ex5_a          = User (fromList [(T0, 0), (T1, 0), (T2, 4)]) "A"
        ex5_init_state = (ex5_amms, [ex5_a])
        ex5_init_conf  = Configuration ex5_init_state ex5_init_state DS.Empty
        (res, log)     = runTransactions False ex5_init_conf ex5_txns ex5_q_len

        -- update this once fixed:
        expected = Configuration 
          -- green
          ([AMM (T0,  8.0) (T1, 18.0),
            AMM (T2, 12.0) (T1, 12.0) ],
            [User (fromList [(T0, 8.0),(T1, 0.0),(T2, -6.0)]) "A"])
          -- simulated
          ([AMM (T0,  8.0) (T1, 18.0),
            AMM (T2, 12.0) (T1, 12.0) ],
            [User (fromList [(T0, 8.0),(T1, 0.0),(T2, -6.0)]) "A"])
          -- queue
          (DS.Empty)
        err_message = unlines (log ++ ["expected conf:"] ++ [(show expected)] ++ ["but got:"] ++ [(show res)])

    assertBool err_message (res == expected)
    return $ unlines log