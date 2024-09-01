module Netting.Worth where

import Netting.Sem
import Netting.AmmFuns
import qualified Data.Map as DM
import Data.Maybe


-- can be used to provision AMMs and txns with "real" exchange rates
oracleXRate :: Token -> Token -> Float
oracleXRate WBTC WBTC = 1.0
oracleXRate WBTC WETH = 22.70
oracleXRate WBTC USDC = 58793.6

oracleXRate WETH WETH = 1.0 
oracleXRate WETH WBTC = 1.0 / oracleXRate WBTC WETH
oracleXRate WETH USDC = 2596.76

oracleXRate USDC USDC = 1.0
oracleXRate USDC WBTC = 1.0 / oracleXRate WBTC USDC
oracleXRate USDC WETH = 1.0 / oracleXRate WETH USDC


-- netWorth denoted in USDC, calculated using the oracle exchange rate
netWorth :: User -> Float
netWorth (User wallet _) = 
    DM.foldrWithKey (\k v acc -> acc + v * (oracleXRate k USDC)) 0 wallet

-- calcualtes the gain made by 'name' after transaction txn in s
gain :: Transaction -> State -> Maybe Float
gain txn@(Transaction name _ _) s@(_, users) =
    case findUser users name 0 of
        Nothing  -> Nothing -- if user not in s, return Nothing
        Just idx -> 
            let worth = netWorth (users !! idx) in
                let s'@(amms', users') = fromMaybe s (executeSwap s txn) in 
                    let worth' = netWorth (users' !! idx) in -- fine, as executeSwap shouldn't reorder users
                        Just $ worth' - worth

-- considers the arbitrage game on a single CFMM AMM (https://lmcs.episciences.org/10504/pdf, lemma 6.4)
-- returns the 'input swap amount' that brings AMM back into equilibrium with oracle rate
singleAmmArbitrage :: AMM -> (TokenAmt, TokenAmt)
singleAmmArbitrage amm@(AMM (t0, r0) (t1, r1)) =
    if r1 / r0 > oracleXRate t0 t1 then 
        -- t1 is cheap compared to oracle rate, thus swap(t0, t1) gives pos gain
        let v0 = sqrt (oracleXRate t1 USDC / oracleXRate t0 USDC * r0 * r1) - r0 in 
            ((t0, v0), payout amm (t0, v0))
    else 
        -- t0 is cheap compared to oracle rate, thus swap(t1, t0) gives pos gain
        let v1 = sqrt (oracleXRate t0 USDC / oracleXRate t1 USDC * r1 * r0) - r1 in 
            ((t1, v1), payout amm (t1, v1))
    where 
        payout (AMM (t0, r0) (t1, r1)) (t2, v0) -- payout that preserves CFMM properties
            | t0 == t2  = (t1, (r1 * v0) / (r0 + v0)) 
            | otherwise = (t0, (r0 * v0) / (r1 + v0))
            