module Main where

import Control.Monad
import Data.Map
import Data.Maybe
import Netting.Sem
import Netting.Funcs
import System.Random
import qualified Data.Sequence as DS

ex0_amms :: [AMM]
ex0_amms = 
    let wbtcXweth = AMM (WBTC, 12) (WETH, 12) in 
    let wethXusdc = AMM (WETH, 12) (USDC, 12) in 
        [wbtcXweth, wethXusdc]

ex0_txns :: [Transaction]
ex0_txns = 
    let s1 = Transaction "B" (WETH, 6) (USDC, 4) in
    let s2 = Transaction "A" (WETH, 6) (WBTC, 4) in
    let s3 = Transaction "A" (USDC, 4) (WETH, 6) in
    let s4 = Transaction "B" (WBTC, 4) (WETH, 6) in
        [s1, s2, s3, s4]

ex5_amms :: [AMM]
ex5_amms = 
    let wbtcXweth = AMM (WBTC, 12) (WETH, 12) in 
    let wethXusdc = AMM (WETH, 18) (USDC, 8) in 
    let usdcXwbtc = AMM (USDC, 12) (WBTC, 12) in 
        [wbtcXweth, wethXusdc, usdcXwbtc]

ex5_txns :: [Transaction]
ex5_txns = 
    let s1 = Transaction "A" (USDC, 6) (WBTC, 4)  in
    let s2 = Transaction "A" (WETH, 6) (WBTC, 4)   in
    let s3 = Transaction "A" (USDC, 4) (WETH, 6) in
        [s1, s2, s3]


exec_txns :: Configuration -> [Transaction] -> Int -> Int -> IO Configuration
exec_txns conf (txn:txns) max_q_len i = do
    let conf' = fromMaybe conf (takeStep conf txn max_q_len)
    print $ "Config after swap " ++ (show i) ++ ":"
    print $ conf'
    exec_txns conf' txns max_q_len (i+1)
exec_txns conf [] max_q_len i = pure conf

main :: IO ()
main = do
    print "Example 0:"

    -- first example from section 2 of paper
    let ex0_q_len  = 8
    let ex0_a = User (fromList [(WBTC, 0), (WETH, 0), (USDC, 4)]) "A"
    let ex0_b = User (fromList [(WBTC, 4), (WETH, 0), (USDC, 0)]) "B"
    let ex0_init_state = (ex0_amms, [ex0_a, ex0_b])
    let ex0_init_conf  = Configuration ex0_init_state ex0_init_state DS.Empty
    ex0_res <- exec_txns ex0_init_conf ex0_txns ex0_q_len 0

    print "Example 5: "

    -- (Netting a short queue, discarding first transaction)
    let ex5_q_len = 2
    let ex5_user = User (fromList [(WBTC, 0), (WETH, 0), (USDC, 4)]) "A"
    let ex5_init_state = (ex5_amms, [ex5_user])
    let ex5_init_conf  = Configuration ex5_init_state ex5_init_state DS.Empty
    ex5_res <- exec_txns ex5_init_conf ex5_txns ex5_q_len 0
    pure ()