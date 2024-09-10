module Netting.Sem where

import qualified Data.Map as DM
import qualified Data.Sequence as DS

data Token
  = T0
  | T1
  | T2
  deriving (Show, Eq, Ord)

type Balance = DM.Map Token Float

data User = User 
  { wallet  :: Balance,
    name    :: String }
    deriving (Show, Eq)

type TokenAmt = (Token, Float)

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

-- To simplify matters, a transaction only contains the String name
-- of the sender, and not a 'User' object, as that would require maintaining 
-- the User.wallet in the transaction in addition to the states.
data Transaction = Transaction
  { sender :: String,
    from   :: TokenAmt,
    to     :: TokenAmt } 
    deriving (Show, Eq)

type Queue = DS.Seq Transaction

type QLength = Int

data Configuration = Configuration
  { lastGreen :: State,
    simulated :: State,
    queue     :: Queue }
    deriving (Show, Eq)