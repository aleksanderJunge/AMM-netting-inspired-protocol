module Netting.AmmFuns where

import qualified Data.Map as DM
import Netting.Sem
import Data.Sequence (Seq ((:|>), (:<|))) -- explicit import of insertion operators
import qualified Data.Sequence as DS      -- qualified import of rest of lib
import Data.List
import Data.Either
import Data.Foldable

-- May result in a negative balance of the user
swap :: AMM -> Transaction -> Balance -> Either String (AMM, User)
swap amm@(AMM (t0, r0) (t1, r1)) txn@(Transaction name (t2, v0) (t3, v1)) wallet =
  if t0 /= t2 then
    swap (AMM (t1, r1) (t0, r0)) txn wallet
  else
    if (0 <= payout) && (payout >= v1) then 
      Right (AMM (t0, r0 + v0) (t1, r1 - payout), User newbal name)
    else 
      Left $ name ++ " couldn't swap " ++ (show v0) ++ " " ++ (show t0) ++ " for " 
                  ++ (show v1) ++ " " ++ (show t1) ++ ", AMM payout for this input would be: " 
                  ++ (show payout) ++ " " ++ (show t1) ++ ". "
  where 
    payout = (r1 * v0) / (r0 + v0) -- preserves CFMM properties
    newbal = DM.insert t1 ((getBal t1 wallet) + payout) $ DM.insert t0 ((getBal t0 wallet) - v0) wallet

-- this executes a swap on the provided state and returns a new state
executeSwap :: State -> Transaction -> Either String State
executeSwap s@(amms, users) txn@(Transaction name (t0, _) (t1, _)) =
  case findAMM amms t0 t1 of
    Just i -> -- AMM at index i matches token pair
      case findUser users name of 
        Just j -> -- User at index j matches name
          let (head_amms, amm:tail_amms )              = splitAt i amms
              (head_users, (User wal name):tail_users) = splitAt j users
          in
            case swap amm txn wal of
              Left message              -> Left message
              Right (new_amm, new_user) -> Right $ (head_amms ++ new_amm : tail_amms, head_users ++ new_user : tail_users)
        Nothing -> Left "no User's name matched txn sender"
    Nothing     -> Left "no AMM matches this swap token pair"

-- Takes a state and a queue of txns and finds a solution to the 'netting-problem'
net :: (Queue -> State -> Int) -> State -> Queue -> Queue
net findOverdraft s q@(txn :<| txns) = 
  let s' = runQueue q s in 
    if isGreen s' then q
    else
      let violating_idx = findOverdraft q s
          q'            = DS.deleteAt violating_idx q
      in 
        net findOverdraft s q'
net _ _ DS.Empty = DS.Empty

-- state machine only supports swaps, TODO: implement redeem/deposit
takeStep :: (Configuration, Int, [String]) -> Transaction -> QLength -> Bool -> (Configuration, Int, [String])
takeStep (conf@(Configuration green sim queue), i, log) txn maxqlen runInMaxOverdraftMode =
  let s' = executeSwap sim txn in
  case executeSwap sim txn of 
    Left message -> (conf, i + 1, (add_to_log (" not executed becase " ++ message) sim i) : log)
    Right s'     ->
      if isGreen s' then (Configuration s' s' DS.Empty, i + 1, (add_to_log " applied cover rule " sim i) : log)
      else 
        if DS.length queue < maxqlen then
          (Configuration green s' (queue :|> txn), i + 1, (add_to_log " applied overdraft rule" sim i) : log)
        else
          let nettingAlgo = if runInMaxOverdraftMode then findLargestOverdraft else findFirstOverdraft
              q'          = net nettingAlgo green (queue :|> txn)
              s''         = runQueue q' green 
          in
            (Configuration s'' s'' DS.Empty, i + 1, 
              (add_to_log (" applied netting rule on queue: \n" ++ (show . toList $ queue :|> txn) ++ "\n") sim i) : log)
  where 
    add_to_log message s i = 
      "\ntxn " ++ (show i) ++ ": " ++ (show txn) ++ message ++ " in state:\n" ++ (show s) ++ "\n"


-- used to run a sequence of transactions on a configuration
-- outputs (the configuration after executing txns, a log with a brief summary of each txn)
runTransactions :: Bool -> Configuration -> [Transaction] -> QLength -> (Configuration, [String])
runTransactions runNetInMaxOverdraftMode conf txns maxqlen =
  let (conf', _, log) = foldl' (\acc txn -> takeStep acc txn maxqlen runNetInMaxOverdraftMode) (conf, 1, []) txns
  in (conf', "LOG: " : reverse log)

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
  let s' = fromRight s (executeSwap s txn)
  in runQueue txns s'
runQueue DS.Empty s = s

-- takes a queue and a state, and finds the first transaction in the queue that results in a red state
findFirstOverdraft :: Queue -> State -> Int
findFirstOverdraft q s =
  find_idx q s 0
  where 
    find_idx (txn :<| txns) s' i = case executeSwap s' txn of
                                  Left _  -> i
                                  Right s'' -> if isGreen s'' then find_idx txns s'' (i+1) else i

-- takes a queue and a state, returns index of transaction in queue that causes the biggest overdraft
findLargestOverdraft :: Queue -> State -> Int
findLargestOverdraft q s =
  find_idx q s 0 0
  where 
    find_idx DS.Empty _ _ _ = 0 -- unreachable, as there will be some txn in q giving negative balance when this is called
    find_idx (txn :<| txns) s i lo = 
      case executeSwap s txn of
        Left _ -> i
        Right s'@(_, users) -> 
          if isGreen s' then find_idx txns s' (i+1) lo 
          else
            case findUser users (sender txn) of
              Nothing  -> i
              Just idx -> let name      = sender txn
                              bal       = wallet $ users !! idx
                              token     = fst (from txn)
                              overdraft = getBal token bal
                          in 
                            find_idx txns s' (i+1) (min lo overdraft)

-- TODO: Make AMM an instance of eq instead of this boguspokus
-- returns the index of the AMM corresponding to the Token pair (or Nothing)
findAMM :: [AMM] -> Token -> Token -> Maybe Int
findAMM amms t0 t1 =
  find_amm amms t0 t1 0
  where 
    find_amm [] _ _ _ = Nothing
    find_amm (amm@(AMM (t0, r0) (t1, r1)):amms) t2 t3 i =
      if (t0 == t2 && t1 == t3) || (t1==t2 && t0 == t3) then
        Just i
      else
        find_amm amms t2 t3 (i + 1)
    
-- returns the index of the User corresponding to the name (or Nothing)
findUser :: [User] -> String -> Maybe Int
findUser users lfName =
  find_user users lfName 0
  where 
    find_user [] _ _ = Nothing
    find_user ((User _ name):users) lfName i = 
      if name == lfName then
        Just i
      else
        find_user users lfName (i + 1)