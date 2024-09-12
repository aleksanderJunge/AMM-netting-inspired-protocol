{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
module Netting.AmmFuns where

import qualified Data.Map as M
import Netting.Sem
import Data.Sequence (Seq ((:|>), (:<|))) -- explicit import of insertion operators
import qualified Data.Sequence as S      -- qualified import of rest of lib
import Data.List
import Data.Either
import Data.Foldable
import Data.Maybe
import Control.Monad

-- May result in a negative balance of the user
swap :: AMM -> Swap -> Balance -> Either String (AMM, User)
swap amm@(AMM (t0, r0) (t1, r1)) txn@(Swap name (t2, v0) (t3, v1)) bal =
  if t0 /= t2 then
    swap (AMM (t1, r1) (t0, r0)) txn bal
  else
    if (0 <= payout) && (payout >= v1) then 
      let bal'  = updateBal t1 payout
                . updateBal t0 (- v0) $ bal
      in
      Right (AMM (t0, r0 + v0) (t1, r1 - payout), User bal' name)
    else 
      Left $ name ++ " couldn't swap " ++ (show v0) ++ " " ++ (show t0) ++ " for " 
                  ++ (show v1) ++ " " ++ (show t1) ++ ", AMM payout for this input would be: " 
                  ++ (show payout) ++ " " ++ (show t1) ++ ". "
  where 
    payout = (r1 * v0) / (r0 + v0) -- preserves CFMM properties
    updateBal t v bal = M.insert (AtomTok t) (getBal (AtomTok t) bal + v) bal

-- this executes a swap on the provided state and returns a new state
executeSwap :: State -> Swap -> Either String State
executeSwap s@(amms, users) txn@(Swap name (t0, _) (t1, _)) =
  case findAMMIdx amms t0 t1 of
    Nothing -> Left "no AMM matches this swap token pair"
    Just i  -> -- AMM at index i matches token pair
      case findUserIdx users name of 
        Nothing -> Left "no User's name matched txn sender"
        Just j  -> -- User at index j matches name
          let (head_amms, amm:tail_amms )              = splitAt i amms
              (head_users, (User wal name):tail_users) = splitAt j users
          in
            case swap amm txn wal of
              Left message              -> Left message
              Right (new_amm, new_user) -> Right $ (head_amms ++ new_amm : tail_amms, head_users ++ new_user : tail_users)

executeDeposit :: State -> Deposit -> Either String State
executeDeposit s@(amms, users) dep@(Deposit name (t0, v0) (t1, v1))
  | v0 <= 0 || v1 <= 0 = Left "Must deposit > 0"
  | canAfford          = 
      case findUserIdx users name of 
        Nothing -> Left "User depositing doesn't exist in state"
        Just i  -> 
          let (head_users, u@(User w _):tail_users) = splitAt i users
              supply = tokenSupply s (MintTok $ MT (t0, t1)) 
          in
            if supply == 0 then -- [deposit0]
              let amm    = AMM (t0, v0) (t1, v1)
                  bal'   = updateBal (AtomTok t0) (-v0)
                         . updateBal (AtomTok t1) (-v1)
                         . updateBal (MintTok $ MT (t0, t1)) v0 $ w
              in Right (amms ++ [amm], head_users ++ (User bal' name) : tail_users)
            else -- [deposit1]
              case findAMMIdx amms t0 t1 of 
                Nothing -> Left $ " AMM for: " ++ (show t0) ++ " " ++ (show t1) ++ " doesn't exist.(shouldn't be possible)"
                Just j  -> 
                  let (head_amms, (AMM (t0', r0) (t1', r1)):tail_amms ) = splitAt j amms in
                    if not (r0 * v1 == r1 * v0) then -- TODO: these are floats ... what is the precision of this equality?
                      Left $ "Deposit doesn't preserve ratio between reserves " ++ (show dep)
                    else 
                      let payout = v0 / r0 * supply
                          amm'   = (AMM (t0, r0 + v0) (t1, r1 + v1))
                          bal'   = updateBal (AtomTok t0) (-v0) 
                                 . updateBal (AtomTok t1) (- v1)
                                 . updateBal (MintTok $ MT (t0, t1)) payout $ w
                      in Right (head_amms ++ amm' : tail_amms, head_users ++ (User bal' name) : tail_users)
  | otherwise = Left $ "User " ++ name ++ " can't afford to execute " ++ (show dep)
  where 
    updateBal t v bal = M.insert t ((getBal t bal) + v) bal
    canAfford =
      let userBal = fromMaybe M.empty (liftM (\(User w _) -> w) (findUser name users))
          v0Bal   = getBal (AtomTok t0) userBal
          v1Bal   = getBal (AtomTok t1) userBal 
      in v0Bal >= v0 && v1Bal >= v1

executeRedeem :: State -> Redeem -> Either String State
executeRedeem swp dep = Left "TODO: Implement me"


-- Takes a state and a queue of txns and finds a solution to the 'netting-problem'
net :: (Queue -> State -> Int) -> State -> Queue -> Queue
net findOverdraft s q@(txn :<| txns) = 
  let s' = runQueue q s in 
    if isGreen s' then q
    else
      let violating_idx = findOverdraft q s
          q'            = S.deleteAt violating_idx q
      in 
        net findOverdraft s q'
net _ _ S.Empty = S.Empty

takeStep :: (Configuration, Int, [String]) -> TransactionT -> QLength -> Bool -> (Configuration, Int, [String])
takeStep (conf@(Configuration green sim queue), i, log) txn maxqlen runInMaxOverdraftMode =
  case txn of 
    Swp swp -> second_lvl_sem $ executeSwap sim swp
    Dep dep -> second_lvl_sem $ executeDeposit sim dep
    Rdm (Redeem  n v )    -> (conf, 0, ["Redeem not implemented yet!"])
  where 
    add_to_log message s i = 
      "\ntxn " ++ (show i) ++ ": " ++ (show txn) ++ message ++ " in state:\n" ++ (show s) ++ "\n"
    second_lvl_sem =
      \case
        Left message -> (conf, i + 1, (add_to_log (" not executed becase " ++ message) sim i) : log)
        Right s'     -> 
          if isGreen s' then (Configuration s' s' S.Empty, i + 1, (add_to_log " applied cover rule " sim i) : log)
          else 
            if S.length queue < maxqlen then
              (Configuration green s' (queue :|> txn), i + 1, (add_to_log " applied overdraft rule" sim i) : log)
            else
              let nettingAlgo = if runInMaxOverdraftMode then findLargestOverdraft else findFirstOverdraft
                  q'          = net nettingAlgo green (queue :|> txn)
                  s''         = runQueue q' green 
              in
                (Configuration s'' s'' S.Empty, i + 1, 
                  (add_to_log (" applied netting rule on queue: \n" ++ (show . toList $ queue :|> txn) ++ "\n") sim i) : log)


-- used to run a sequence of transactions on a configuration
-- outputs (the configuration after executing txns, a log with a brief summary of each txn)
runTransactions :: Bool -> Configuration -> [TransactionT] -> QLength -> (Configuration, [String])
runTransactions runNetInMaxOverdraftMode conf txns maxqlen =
  let (conf', _, log) = foldl' (\acc txn -> takeStep acc txn maxqlen runNetInMaxOverdraftMode) (conf, 1, []) txns
  in (conf', "LOG: " : reverse log)


--- More auxillary functions follows below ---

isGreen :: State -> Bool
isGreen (_, users) = 
  (==0) . length $ filter (\(User w _) -> is_neg w) users
  where is_neg = not . M.null . M.filter (< 0.0)

getBal :: TokenT -> Balance -> Float
getBal = M.findWithDefault 0.0

-- Runs all the transactions in the queue and returns a new state
runQueue :: Queue -> State -> State
runQueue (txn :<| txns) s =
  let s' = case txn of 
            Swp swp@(Swap n v0 v1)    -> fromRight s (executeSwap s swp)
            Dep dep@(Deposit n v0 v1) -> fromRight s (executeDeposit s dep)
            Rdm rdm@(Redeem  n v )    -> s
  in runQueue txns s'
runQueue S.Empty s = s

-- takes a queue and a state, and finds the first transaction in the queue that results in a red state
findFirstOverdraft :: Queue -> State -> Int
findFirstOverdraft q s =
  find_idx q s 0
  where 
    find_idx (txn :<| txns) s i =
      case txn of 
        Swp swp -> 
          case executeSwap s swp of
            Left _  -> i
            Right s' -> if isGreen s' then find_idx txns s' (i+1) else i
        Dep dep -> 
          case executeDeposit s dep of 
            Left _  -> i
            Right s' -> if isGreen s' then find_idx txns s' (i+1) else i
        Rdm rdm -> i -- TODO : Fix when supported

-- takes a queue and a state, returns index of transaction in queue that causes the biggest overdraft
findLargestOverdraft :: Queue -> State -> Int
findLargestOverdraft q s =
  find_idx q s 0 0
  where 
    find_idx S.Empty _ _ _ = 0 -- unreachable, as there will be some txn in q giving negative balance when this is called
    find_idx (txn :<| txns) s i lo = 
      case txn of 
        Swp swp@(Swap n v0 v1) -> 
          case executeSwap s swp of
            Left _  -> i
            Right s'@(_, users) -> 
              if isGreen s' then find_idx txns s' (i+1) lo 
              else
                case findUserIdx users (sender swp) of
                  Nothing  -> i
                  Just idx -> let name      = sender swp
                                  bal       = wallet $ users !! idx
                                  token     = fst (from swp)
                                  overdraft = getBal (AtomTok token) bal
                              in 
                                find_idx txns s' (i+1) (min lo overdraft)
        Dep dep@(Deposit n v0' v1') -> 
          case executeDeposit s dep of 
            Left _  -> i
            Right s'@(_, users) -> 
              if isGreen s' then find_idx txns s' (i+1) lo 
              else
                case findUserIdx users (depositor dep) of
                  Nothing  -> i
                  Just idx -> let name       = depositor dep
                                  bal        = wallet $ users !! idx
                                  token1     = fst (v0 dep)
                                  token2     = fst (v1 dep) 
                                  -- deposits shouldn't be able to overdraft, but...
                                  overdraft1 = getBal (AtomTok token1) bal
                                  overdraft2 = getBal (AtomTok token1) bal
                              in 
                                find_idx txns s' (i+1) (min lo (min overdraft1 overdraft2))
        Rdm rdm@(Redeem  n v )  -> i -- TODO : Fix when supported

-- returns the index of the AMM corresponding to the AtomicToken pair (or Nothing)
findAMMIdx :: [AMM] -> AtomicToken -> AtomicToken -> Maybe Int
findAMMIdx amms t0 t1 =
  find_amm amms t0 t1 0
  where 
    find_amm [] _ _ _ = Nothing
    find_amm (amm@(AMM (t0, r0) (t1, r1)):amms) t2 t3 i =
      if (t0 == t2 && t1 == t3) || (t1==t2 && t0 == t3) then
        Just i
      else
        find_amm amms t2 t3 (i + 1)
    
-- returns the index of the User corresponding to the name (or Nothing)
findUserIdx :: [User] -> String -> Maybe Int
findUserIdx users lfName =
  find_user users lfName 0
  where 
    find_user [] _ _ = Nothing
    find_user ((User _ name):users) lfName i = 
      if name == lfName then
        Just i
      else
        find_user users lfName (i + 1)

findUser :: String -> [User] -> Maybe User
findUser name = find (\(User _ n) -> (n == name))

tokenSupply :: State -> TokenT -> Float
tokenSupply (amms, users) (AtomTok t) = 
  let amm_supply = sum $ map (\(AMM (t0, v0) (t1, v1)) -> 
                              if
                              | t0 == t   -> v0 
                              | t1 == t   -> v1
                              | otherwise -> 0 ) amms
      user_supply = sum $ map (\(User w _) -> getBal (AtomTok t) w) users
  in amm_supply + user_supply

tokenSupply (_, users) mt@(MintTok t) =
  sum $ map (\(User w _) -> getBal mt w) users
