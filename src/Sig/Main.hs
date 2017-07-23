module Sig.Main where

import Sig (State(..))

import qualified Control.Concurrent
import qualified Sig
import qualified Sig.Examples
import qualified System.IO.MMap

main :: IO ()
main = do
    k     <- Control.Concurrent.getNumCapabilities
    bytes <- System.IO.MMap.mmapFileByteString "test.txt" Nothing
    let transition = Sig.runInParallel k Sig.Examples.cStyleComments bytes
    print (Sig.fromState00To transition == S13)
