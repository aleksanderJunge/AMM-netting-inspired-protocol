module Main where

import Control.Monad
import Data.Map
import Data.Maybe
import Util 
import Sem
import System.Random
import qualified Data.Sequence as DS

-- amms are initialized with 2 * 1321 BTC worth of assets (around 150 mn USD)
initialAMMs :: [AMM]
initialAMMs = 
    let wbtcXweth = AMM (WBTC, 1321) (WETH, 1321 * oracleXRate WBTC WETH) in 
    let wbtcXusdc = AMM (WBTC, 1321) (USDC, 1321 * oracleXRate WBTC USDC) in 
    let wethXusdc = AMM (WETH, 1321 * oracleXRate WBTC WETH) (USDC, 1321 * oracleXRate WBTC USDC) in 
        [wbtcXweth, wbtcXusdc, wethXusdc]

exampleAMMs :: [AMM]
exampleAMMs = 
    let wbtcXweth = AMM (WBTC, 12) (WETH, 12) in 
    let wethXusdc = AMM (WETH, 12) (USDC, 12) in 
        [wbtcXweth, wethXusdc]

main :: IO ()
main = do
    --let txn_1 = Transaction aleksander (WBTC, 1.0) (WETH, 1.0 * oracleXRate WBTC WETH * 0.999)
    --let init_amms  = initialAMMs
    --let init_state = (init_amms, [aleksander])
    let max_q_len  = 2
    let a = User (fromList [(WBTC, 0), (WETH, 0), (USDC, 4)]) "A"
    let b = User (fromList [(WBTC, 4), (WETH, 0), (USDC, 0)]) "B"
    let s1 = Transaction "B" (WETH, 6) (USDC, 4)
    let s2 = Transaction "A" (WETH, 6) (WBTC, 4)
    let s3 = Transaction "A" (USDC, 4) (WETH, 6)
    let s4 = Transaction "B" (WBTC, 4) (WETH, 6)
    
    let init_state = (exampleAMMs, [a, b])

    let init_conf  = Configuration init_state init_state DS.Empty

    let conf1 = fromMaybe (init_conf) (takeStep init_conf s1 max_q_len)
    let conf2 = fromMaybe (conf1) (takeStep conf1 s2 max_q_len)
    let conf3 = fromMaybe (conf2) (takeStep conf2 s3 max_q_len)
    let conf4 = fromMaybe (conf3) (takeStep conf3 s4 max_q_len)
    print "INITIAL"
    print $ init_conf
    print "CONF: 1"
    print $ conf1
    print "CONF: 2"
    print $ conf2
    print "CONF: 3"
    print $ conf3
    print "CONF: 4"
    print $ conf4


--rollDice = getStdRandom (randomR (1, 6))
--initStdGen
--num <- randomIO :: IO Int
---- This "extracts" the float from IO Float and binds it to the name num
--nums <- replicateM 10 (rollDice :: IO Int)