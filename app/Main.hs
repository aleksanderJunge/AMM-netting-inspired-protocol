module Main where

import Control.Monad
import Data.Map
import Data.Maybe
import Netting.Sem
import Netting.AmmFuns
import Netting.Sim
import Netting.Worth
import qualified Data.Sequence as DS
import Data.Foldable


main :: IO ()
main = do
    print "Nothing interesting here yet, run 'cabal run test:tests' to see examples"

    -- Gridlock example (user with 0 balance arbitrage across 3 amms)

    --Configuration g _ _ <- exec_txns ex1_init_conf ex1_txns ex1_q_len 0

    --print $ "executing swap(6 t0, 4 t1), A's gain is: " ++ show (fromMaybe 0 (txnGain (ex1_txns !! 0) ex1_init_state)) ++ " T2"
    --print $ "gain after example 1 of user A is: " ++ show (gain "A" ex1_init_state g) ++ " T2"

    --print "Example 5: "

    ---- (Netting a short queue, discarding first transaction)
    --let ex5_q_len = 2
    --let ex5_user = User (fromList [(T0, 0), (T1, 0), (T2, 4)]) "A"
    --let ex5_init_state = (ex5_amms, [ex5_user])
    --let ex5_init_conf  = Configuration ex5_init_state ex5_init_state DS.Empty
    --ex5_res <- exec_txns ex5_init_conf ex5_txns ex5_q_len 0
    --pure ()

    --print "On the AMM (T0: 10, T1 40) the swap input amount that brings the AMM into equilibrium is: "
    --print $ singleAmmArbitrage (AMM (T0, 10) (T1, 40))

    --print "On the AMM (T0: 1, T1 40) the swap input amount that brings the AMM into equilibrium is: "
    --print $ singleAmmArbitrage (AMM (T0, 1) (T1, 40))
    
    --let arbTxn = genSingleArbitrageTxn "A" ex1_init_state
    --print "Single highest gain transaction to execute on ex1_amms is :"
    --print arbTxn
    --print "which yields the state :"
    --print $ fromMaybe ex1_init_state (executeSwap ex1_init_state arbTxn)
    --print "in which the t0/t2 amm is in balance with oracle rate"