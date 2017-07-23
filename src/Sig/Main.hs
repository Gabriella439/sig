module Sig.Main where

import qualified Control.Concurrent
import qualified Sig
import qualified Sig.Examples
import qualified System.IO.MMap

main :: IO ()
main = do
    k     <- Control.Concurrent.getNumCapabilities
    bytes <- System.IO.MMap.mmapFileByteString "test.txt" Nothing
    print (Sig.parallelProcess Sig.Examples.cStyleComments k bytes)
