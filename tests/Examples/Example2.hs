{-# LANGUAGE BlockArguments #-}
module Examples.Example2 where

import Test.Tasty
import Test.Tasty.HUnit
import Netting.Sem
import Netting.AmmFuns

import Data.Map
import Data.Foldable
import qualified Data.Sequence as DS

example2 :: TestTree
example2 = 
  testCaseInfo "example 1 \"Negative Balance Covered\"\n" do

    -- TODO: implement once deposit/redeem are added
    assertBool "Not implemented yet" False
    return "Not implemented yet"
