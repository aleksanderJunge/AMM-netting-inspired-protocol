module Netting.Sim where

-- A simulation library for generating transactions and executions

import Netting.Sem
import Netting.AmmFuns
import System.Random

-- generate a random float in range [from, to] (from can't be negative)
genInRange :: (Float, Float) -> IO Float
genInRange (from, to) = getStdRandom (randomR (from, to))
  

-- TODO: Add functions to generate transactions
