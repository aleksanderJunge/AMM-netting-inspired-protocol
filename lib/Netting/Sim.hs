module Netting.Sim where

-- A simulation library for generating transactions and executions

import Netting.Sem
import Netting.AmmFuns
import Netting.Worth
import System.Random
import Data.List
import Data.Maybe

-- generate a random float in range [from, to]
genInRange :: (Float, Float) -> IO Float
genInRange (from, to) = getStdRandom (randomR (from, to))
  
-- constructs the single highest gain transaction for the user 'name' in state s (not caring if he can afford to execute it)
genSingleArbitrageTxn :: String -> State -> Transaction
genSingleArbitrageTxn name s@(amms, users) =
  let input_amounts = map singleAmmArbitrage amms in
    let output_amounts = zipWith payout amms input_amounts in 
      let transactions = zipWith (\x y -> Transaction name x y) input_amounts output_amounts in 
        let gains = map (flip gain s) transactions in 
          transactions !! fromMaybe 0 (elemIndex (maximum gains) gains)
  where 
    payout (AMM (t0, r0) (t1, r1)) (t2, v0) -- payout that preserves CFMM properties
      | t0 == t2  = (t1, (r1 * v0) / (r0 + v0)) 
      | otherwise = (t0, (r0 * v0) / (r1 + v0))

-- TODO: Add more functions to generate transactions