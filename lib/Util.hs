module Util where

import qualified Data.Map as DM
import Sem
import Data.Maybe
import Data.Sequence (Seq ((:|>), (:<|))) -- explicit import of insertion operators
import qualified Data.Sequence as DS      -- qualified import of rest of lib

isGreen :: State -> Bool
isGreen (_, users) = 
  (==0) . length $ filter (\(User w _) -> check_neg w) users
  where check_neg wallet = (not . DM.null) $ DM.filter (< 0.0) wallet

getBal :: Token -> Balance -> Float
getBal = DM.findWithDefault 0.0

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


-- May result in a negative balance of the user
swap :: AMM -> Transaction -> Balance -> (AMM, User)
swap (AMM (t0, r0) (t1, r1)) txn@(Transaction name (t2, v0) (t3, v1)) wallet =
  if t0 /= t2 then
    swap (AMM (t1, r1) (t0, r0)) txn wallet
  else
    (AMM (t0, r0 + v0) (t1, r1 - payout), User newbal name)
  where 
    payout = (r1 * v0) / (r0 + v0) 
    newbal = DM.insert t1 ((getBal t1 wallet) + payout) $ DM.insert t0 ((getBal t0 wallet) - v0) wallet
-- old version had this extra check, and might return Nothing
--swap :: AMM -> Transaction -> Balance -> Maybe (AMM, User)
  --if (0 <= payout) && (payout >= v1) then Nothing...

-- this executes a swap on the provided state and returns a new state
executeSwap :: State -> Transaction -> Maybe State
executeSwap s@(amms, users) txn@(Transaction name (t0, _) (t1, _)) =
  case findAMM amms t0 t1 0 of
    Just i -> -- AMM at index i matches token pair
      case findUser users name 0 of 
        Just j -> -- User at index j matches name
          let (head_amms, amm:tail_amms ) = splitAt i amms in
            let (head_users, (User wal name):tail_users) = splitAt j users in
              let (new_amm, new_user) = swap amm txn wal in
                Just (head_amms ++ new_amm : tail_amms, head_users ++ new_user : tail_users)
        Nothing -> Nothing -- no User matches name in susers
    Nothing -> Nothing --no AMM matches this swap token pair

-- Takes a transaction as input since that is the only supported 'action' at this point of time
takeStep :: Configuration -> Transaction -> QLength -> Maybe Configuration
takeStep conf@(Configuration green sim queue) txn maxqlen =
  let s' = fromMaybe sim (executeSwap sim txn) in -- TODO: probably not best solution to just use sim state if executeswap fails!!
    if isGreen s' then Just (Configuration s' s' DS.Empty)-- Cover rule
    else 
      if DS.length queue < maxqlen then -- Overdraft rule
        Just $ Configuration green s' (queue :|> txn)
      else 
        let s'' = clear_queue green queue in
          Just $ Configuration s'' s'' DS.Empty -- Netting rule (TODO: is this "clear_queue" equivalent to the paper's netting?)


-- Runs all the transactions in the queue and returns a new state
runQueue :: Queue -> State -> State
runQueue (txn :<| txns) s =
  let s' = fromMaybe s (executeSwap s txn) in -- TODO: find a better solution in case executeSwap fails?
    runQueue txns s'
runQueue DS.Empty s = s
    

-- Procedure finds set of transactions in queue that results in green state
--net :: Queue -> State -> Queue
--net q s = 
--  let s' = runQueue q s

-- AUX functions --
-- this function is used to find the first txn in a queue that results in a negative balance for some user
-- returns the state after executing the queue, while removing any overdrafting txns
clear_queue :: State -> Queue -> State
clear_queue s (txn :<| txns) = 
  let s' = fromMaybe s (executeSwap s txn) in
    if isGreen s' then 
      clear_queue s' txns
    else 
      clear_queue s txns
clear_queue s (DS.Empty) = s

-- returns the index of the AMM corresponding to the Token pair (or Nothing)
findAMM :: [AMM] -> Token -> Token -> Int -> Maybe Int
findAMM (amm@(AMM (t0, r0) (t1, r1)):amms) t2 t3 i =
  if (t0 == t2 && t1 == t3) || (t1==t2 && t0 == t3) then
    Just i
  else
    findAMM amms t2 t3 (i + 1)
findAMM [] t2 t3 i = Nothing
    
-- returns the index of the User corresponding to the name (or Nothing)
findUser :: [User] -> String -> Int -> Maybe Int
findUser ((User _ name):users) lfName i =
  if name == lfName then
    Just i
  else
    findUser users lfName (i + 1)
findUser [] lfName i = Nothing