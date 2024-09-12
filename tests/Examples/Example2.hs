{-# LANGUAGE BlockArguments #-}
module Examples.Example2 where

import Test.Tasty
import Test.Tasty.HUnit
import Netting.Sem
import Netting.AmmFuns

import Data.Map
import Data.Foldable
import qualified Data.Sequence as S

example2 :: TestTree
example2 = 
  testCaseInfo "example 2 \"r-swap and AMM creation\"\n" do
    let ex2_amms = 
          [(AMM (T0, 8) (T1, 18))]

        ex2_txns = 
          [ Swp (Swap    "A" (T0, 4) (T1, 6)),
            Dep (Deposit "B" (T1, 6) (T2, 6)),
            Swp (Swap    "B" (T1, 6) (T2, 3)),
            Dep (Deposit "A" (T1, 4) (T2, 1))]

        ex2_q_len      = 2
        ex2_a          = User (fromList [(AtomTok T0, 6), (AtomTok T1,  0), (AtomTok T2, 4)]) "A"
        ex2_b          = User (fromList [(AtomTok T0, 0), (AtomTok T1, 12), (AtomTok T2, 6)]) "B"
        ex2_init_state = (ex2_amms, [ex2_a, ex2_b])
        ex2_init_conf  = Configuration ex2_init_state ex2_init_state S.Empty
        (res, log)     = runTransactions False ex2_init_conf ex2_txns ex2_q_len

        expected = Configuration 
          -- green
          ([AMM (T0, 12.0) (T1, 12.0),
            AMM (T1, 16.0) (T2,  4.0)],
            [User (fromList [(AtomTok T0, 2.0),(AtomTok T1, 2.0),(AtomTok T2, 3.0), (MintTok $ MT(T1, T2), 2.0)]) "A",
             User (fromList [(AtomTok T0, 0.0),(AtomTok T1, 0.0),(AtomTok T2, 3.0), (MintTok $ MT(T1, T2), 6.0)]) "B"])
          -- simulated
          ([AMM (T0, 12.0) (T1, 12.0),
            AMM (T1, 16.0) (T2,  4.0)],
            [User (fromList [(AtomTok T0, 2.0),(AtomTok T1, 2.0),(AtomTok T2, 3.0), (MintTok $ MT(T1, T2), 2.0)]) "A",
             User (fromList [(AtomTok T0, 0.0),(AtomTok T1, 0.0),(AtomTok T2, 3.0), (MintTok $ MT(T1, T2), 6.0)]) "B"])
          -- queue
          (S.Empty)
        err_message = unlines (log ++ ["expected conf:"] ++ [(show expected)] ++ ["but got:"] ++ [(show res)])

    assertBool err_message (res == expected)
    return $ unlines log
