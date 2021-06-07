{-# LANGUAGE OverloadedStrings #-}

module Driver where

import qualified Sig
import qualified Sig.Examples     as Examples
import qualified Test.Tasty.HUnit as HUnit

unit_0 :: IO ()
unit_0 = HUnit.assertEqual "" 0 (Sig.runTransition (Sig.run 1 Examples.cStyleComments "") 0)
