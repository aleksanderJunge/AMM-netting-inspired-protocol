{-# LANGUAGE BlockArguments #-}
module Examples.Example6 where

import Test.Tasty
import Test.Tasty.HUnit
import Netting.Sem
import Netting.AmmFuns

import Data.Map
import Data.List
import Data.Foldable
import qualified Data.Sequence as S

example6 :: TestTree
example6 =
  testCaseInfo "example 6 \"Netting Maximum Overdraft\"\n" do
    let ex6_amms =
            [(AMM (T0, 100) (T1, 100)), 
              (AMM (T1,  80) (T2,  30)),
              (AMM (T2,  30) (T0,  80)) ]
        ex6_txns =
            [ Swp( Swap "A" (T2,  5) (T0, 10)),
              Swp( Swap "A" (T1,  8) (T0,  6)),
              Swp( Swap "A" (T2, 10) (T1, 12)),
              Swp( Swap "A" (T0,  7) (T1, 15)),
              Swp( Swap "A" (T2, 12) (T1, 13)),
              Swp( Swap "A" (T0, 15) (T2, 13)),
              Swp( Swap "A" (T1, 20) (T2, 10)) ]
        ex6_q_len      = 4
        ex6_a          = User (fromList [(AtomTok T0, 50), (AtomTok T1, 30), (AtomTok T2, 10)]) "A"
        ex6_init_state = (ex6_amms, [ex6_a])
        ex6_init_conf  = Configuration ex6_init_state ex6_init_state S.Empty
        (res, log)     = runTransactions True ex6_init_conf ex6_txns ex6_q_len

        expected = Configuration 
            -- green
            ([AMM (T0, 12.0) (T1, 12.0),
              AMM (T1, 12.0) (T2, 12.0),
              AMM (T2, 12.0) (T0, 12.0) ],
            [User (fromList [(AtomTok T0, 32.03),(AtomTok T1, 74.04),(AtomTok T2, 0.0)]) "A"])
            -- simulated
            ([AMM (T0, 12.0) (T1, 12.0),
              AMM (T1, 12.0) (T2, 12.0),
              AMM (T2, 12.0) (T0, 12.0)],
            [User (fromList [(AtomTok T0, 4.0),(AtomTok T1, 0.0),(AtomTok T2, 0.0)]) "A"])
            -- queue
            (S.Empty)
        err_message = unlines (log ++ ["expected conf:"] ++ [(show expected)] ++ ["but got:"] ++ [(show res)])

    assertBool err_message (res == expected)
    return $ unlines log