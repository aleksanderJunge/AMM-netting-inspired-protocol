module Netting.Sem where

import qualified Data.Map as DM
import qualified Data.Sequence as DS

data Token
  = WBTC
  | WETH
  | USDC
  deriving (Show, Eq, Ord)

type Balance = DM.Map Token Float

data User = User 
  { wallet  :: Balance,
    name    :: String }
    deriving Show

type TokenAmt = (Token, Float)

data AMM = AMM 
  { r0 :: TokenAmt,
    r1 :: TokenAmt }
    deriving Show

type State = ([AMM], [User])

data Transaction = Transaction
  { sender :: String,
    from   :: TokenAmt,
    to     :: TokenAmt } 
    deriving Show

type Queue = DS.Seq Transaction

type QLength = Int

data Configuration = Configuration
  { lastGreen :: State,
    simulated :: State,
    queue     :: Queue }
    deriving Show