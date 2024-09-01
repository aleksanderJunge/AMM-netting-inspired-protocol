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
gain txn@(Transaction name (t0, v0) (t1, v1)) s@(amms, users) =
    case findUser users name 0 of
        Nothing  -> Nothing -- if user not in s, return Nothing
        Just idx -> 
            let worth = netWorth (users !! idx) in
                let s'@(amms', users') = fromMaybe s (executeSwap s txn) in 
                    let worth' = netWorth (users' !! idx) in -- fine, as executeSwap shouldn't reorder users
                        Just $ worth' - worth