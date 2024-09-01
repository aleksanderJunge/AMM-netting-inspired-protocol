module Netting.AmmFuns where

import qualified Data.Map as DM
import Netting.Sem
import Data.Maybe ( fromMaybe )
import Data.Sequence (Seq ((:|>), (:<|))) -- explicit import of insertion operators
import qualified Data.Sequence as DS      -- qualified import of rest of lib

-- May result in a negative balance of the user
swap :: AMM -> Transaction -> Balance -> (AMM, User)
swap amm@(AMM (t0, r0) (t1, r1)) txn@(Transaction name (t2, v0) (t3, v1)) wallet =
  if t0 /= t2 then
    swap (AMM (t1, r1) (t0, r0)) txn wallet
  else
    if (0 <= payout) && (payout >= v1) then 
      (AMM (t0, r0 + v0) (t1, r1 - payout), User newbal name)
    else 
      (amm, User wallet name) -- TODO: silently discards swap, because v1* rate was too high?
  where 
    payout = (r1 * v0) / (r0 + v0) -- preserves CFMM properties
    newbal = DM.insert t1 ((getBal t1 wallet) + payout) $ DM.insert t0 ((getBal t0 wallet) - v0) wallet

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

-- Takes a state and a queue of txns and finds a solution to the 'netting-problem'
net :: State -> Queue -> Queue
net s q@(txn :<| txns) = 
  let s' = runQueue q s in 
    if isGreen s' then q
    else
      let violating_idx = findFirstViolation q s 0 in
        let q' = DS.deleteAt violating_idx q in
          net s q'

net _ DS.Empty = DS.Empty

-- state machine only supports swaps, TODO: implement redeem/deposit
takeStep :: Configuration -> Transaction -> QLength -> Maybe Configuration
takeStep conf@(Configuration green sim queue) txn maxqlen =
  let s' = fromMaybe sim (executeSwap sim txn) in
    if isGreen s' then Just (Configuration s' s' DS.Empty)-- Cover rule
    else 
      if DS.length queue < maxqlen then -- Overdraft rule
        Just $ Configuration green s' (queue :|> txn)
      else -- Netting rule
        let q' = net green (queue :|> txn) in
          let s'' = runQueue q' green in
          Just $ Configuration s'' s'' DS.Empty


--- More auxillary functions follows below ---

isGreen :: State -> Bool
isGreen (_, users) = 
  (==0) . length $ filter (\(User w _) -> is_neg w) users
  where is_neg = not . DM.null . DM.filter (< 0.0)

getBal :: Token -> Balance -> Float
getBal = DM.findWithDefault 0.0

-- Runs all the transactions in the queue and returns a new state
runQueue :: Queue -> State -> State
runQueue (txn :<| txns) s =
  let s' = fromMaybe s (executeSwap s txn) in
    runQueue txns s'
runQueue DS.Empty s = s

-- takes a queue and a state, and finds the first transaction in the queue that results in a red state
findFirstViolation :: Queue -> State -> Int -> Int
findFirstViolation (txn :<| txns) s i =
  case executeSwap s txn of
    Nothing -> i --txn contained either invalid user or swap between non-supported token pair
    Just s' -> 
      if isGreen s' then findFirstViolation txns s' (i+1)
      else i

-- returns the index of the AMM corresponding to the Token pair (or Nothing)
findAMM :: [AMM] -> Token -> Token -> Int -> Maybe Int
findAMM (amm@(AMM (t0, r0) (t1, r1)):amms) t2 t3 i =
  if (t0 == t2 && t1 == t3) || (t1==t2 && t0 == t3) then
    Just i
  else
    findAMM amms t2 t3 (i + 1)
findAMM [] _ _ _ = Nothing
    
-- returns the index of the User corresponding to the name (or Nothing)
findUser :: [User] -> String -> Int -> Maybe Int
findUser ((User _ name):users) lfName i =
  if name == lfName then
    Just i
  else
    findUser users lfName (i + 1)
findUser [] _ _ = Nothing