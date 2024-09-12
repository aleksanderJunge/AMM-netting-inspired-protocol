module Netting.Sem where

import qualified Data.Map as M
import qualified Data.Sequence as S

data AtomicToken
  = T0
  | T1
  | T2
  deriving (Show, Eq, Ord)

data MintedToken a
  = MT (a, a)
  deriving (Show, Ord)

instance (Eq a) => Eq (MintedToken a) where
  MT (t0, t1) == MT (t2, t3)
    | (t0 == t2 && t1 == t3) || (t0 == t3 && t1 == t2) = True
    | otherwise                                        = False
  mtok1 /= mtok2 = not $ mtok1 == mtok2

type MintedTokenT = MintedToken AtomicToken

-- sort of like "Either", just for tokens
data Token a b = AtomTok a | MintTok b
  deriving (Show, Eq, Ord)

type TokenT = Token AtomicToken MintedTokenT 

type Balance = M.Map TokenT Float

data User = User 
  { wallet  :: Balance,
    name    :: String }
    deriving (Show, Eq)

type TokenAmt = (AtomicToken, Float)

type MintedTokenAmt = (MintedTokenT, Float)

data AMM = AMM 
  { r0 :: TokenAmt,
    r1 :: TokenAmt }
    deriving (Show)

instance Eq AMM where
  AMM r0 r1 == AMM r2 r3 
    | (r0 == r2 && r1 == r3) || (r0 == r3 && r1 == r2) = True
    | otherwise                                        = False
  amm1 /= amm2 = not $ amm1 == amm2

type State = ([AMM], [User])

data Transaction a b c
  = Swp a | Dep b | Rdm c
  deriving (Show, Eq)

type TransactionT
  = Transaction Swap Deposit Redeem

data Swap
  = Swap
  { sender :: String,
    from   :: TokenAmt,
    to     :: TokenAmt }
    deriving (Show, Eq)

data Deposit
  = Deposit
  { depositor :: String,
    v0        :: TokenAmt,
    v1        :: TokenAmt }
    deriving (Show, Eq)

data Redeem
  = Redeem
  { redeemer :: String,
    v        :: MintedTokenAmt }
    deriving (Show, Eq)

type Queue = S.Seq TransactionT

type QLength = Int

data Configuration = Configuration
  { lastGreen :: State,
    simulated :: State,
    queue     :: Queue }
    deriving (Show, Eq)