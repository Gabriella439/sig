{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeOperators      #-}

module Sig.Main where

import Filesystem.Path (FilePath)
import Options.Generic (Generic, ParseRecord, Wrapped, type (<?>), (:::))
import Prelude hiding (FilePath)
import Sig (State(..), Transition(..))

import qualified Control.Concurrent
import qualified Filesystem.Path.CurrentOS
import qualified Options.Generic
import qualified Sig
import qualified Sig.Examples
import qualified System.IO.MMap

newtype Options w = Options
    { path :: w ::: FilePath <?> "Path to file to test"
    } deriving (Generic)

instance ParseRecord (Options Wrapped)

main :: IO ()
main = do
    let description = "Benchmark Sig performance on a sample file"
    Options {..} <- Options.Generic.unwrapRecord description
    numThreads   <- Control.Concurrent.getNumCapabilities
    let pathString = Filesystem.Path.CurrentOS.encodeString path
    bytes <- System.IO.MMap.mmapFileByteString pathString Nothing
    -- The choice of `StateMachine` does not matter.  This library takes the
    -- same time to match an input regardless of the state machine specification
    let transition = Sig.run numThreads Sig.Examples.cStyleComments bytes
    print (runTransition transition S00 == S00)
