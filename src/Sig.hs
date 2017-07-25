{-| This library provides a very efficient implementation of parallel state
    machines running over `ByteString` inputs based on the following paper:

    > Mytkowicz, Todd, Madanlal Musuvathi, and Wolfram Schulte. "Data-parallel
    > finite-state machines." ACM SIGARCH Computer Architecture News. Vol. 42.
    > No. 1. ACM, 2014.

    These state machines are \"parallel\" in two senses of the word:

    * You can simulate multiple states in parallel
    * You can process the input `ByteString` itself in parallel

    This state machine implementation gives excellent performance on the order
    of 1 GB\/s\/core and the performance scales linearly with the number of
    available cores because the algorithm is embarrassingly parallel.

    The main limitation of this library is that the state machines are limited
    to 16 states
-}

module Sig
    ( -- * Example
      -- $example

      -- * Implementation
      -- $implementation

      -- * Running state machines
      run
    , runInParallel

      -- * Types
    , buildStateMachine
    , StateMachine(..)
    , Transition(..)
    , State(..)
    ) where

import Data.ByteString (ByteString)
import Foreign (Ptr)
import Foreign.C.Types (CChar(..), CSize(..))
import Sig.State
import Sig.StateMachine
import Sig.Transition

import qualified Control.Parallel.Strategies
import qualified Data.Binary
import qualified Data.ByteString
import qualified Data.ByteString.Lazy
import qualified Data.ByteString.Unsafe
import qualified Foreign
import qualified Foreign.Marshal.Unsafe

-- $example
-- 
-- Here is an example of how you would define a state machine that parses
-- C-style block comments using four states (which you can find in the
-- "Sig.Examples" module):
--
-- * `S00` - Starting state and final state for a well-formed comment
-- * `S01` - Just parsed a @\'/\'@ that might be the first character in @\"/*\"@
-- * `S02` - In the middle of a block comment
-- * `S03` - Just parsed a @\'*\'@ that might be the first character in @\"*/\"@
--
-- > {-# LANGUAGE RecordWildCards #-}
-- > 
-- > import Sig (State(..), StateMachine)
-- > 
-- > import qualified Sig
-- > 
-- > cStyleComments :: StateMachine
-- > cStyleComments = Sig.buildStateMachine f
-- >   where
-- >     -- 47 is the ASCII encoding for '/'
-- >     f 47 S00 = S01
-- >     f 47 S01 = S01
-- >     f 47 S02 = S02
-- >     f 47 S03 = S00
-- >
-- >     -- 42 is the ASCII encoding for '*'
-- >     f 42 S00 = S00
-- >     f 42 S01 = S02
-- >     f 42 S02 = S03
-- >     f 42 S03 = S03
-- >
-- >     -- This covers all other ASCII characters
-- >     f  _ S00 = S00
-- >     f  _ S01 = S00
-- >     f  _ S02 = S02
-- >     f  _ S03 = S02
-- > 
-- >     -- This covers all other states (which we don't use)
-- >     f  _ _   = S00
--
-- ... and here is an example of using the above state machine on a file:
--
-- > import Sig (State(..))
-- >
-- > import qualified Control.Concurrent
-- > import qualified Sig
-- > import qualified Sig.Examples
-- > import qualified System.IO.MMap
-- >
-- > main :: IO ()
-- > main = do
-- >     n     <- Control.Concurrent.getNumCapabilities
-- >     bytes <- System.IO.MMap.mmapFileByteString "example.c" Nothing
-- >     let transition = Sig.runInParallel n Sig.Examples.cStyleComments bytes
-- >     print (Sig.fromState00To transition == S00)

{- $implementation
 
    This algorithm revolves around the `Transition` type, which represents a
    single step of the state machine.  For each possible starting state, a
    `Transition` type specifies the next state to transition to.

    This `Transition` type is a `Monoid` where:

    * `mempty` is a `Transition` that does nothing (every state transitions to
      itself)
    * `mappend` combines two `Transition`s end-to-end (i.e. run the first
      transition and then run the second transition)

    We can take advantage of the fact that `Transition` is a `Monoid` to
    simulate the state machine in parallel.  We compute the `Transition` for
    each byte and then use a parallel `mconcat` to derive the `Transition` for
    the entire `ByteString`.

    Normally this parallel state machine simulation would be less efficient than
    a single-threaded state machine simulation.  The naïve implementation of
    `mappend` is much slower than single-stepping a state machine, so you would
    need a large number of processors before the parallel implementation caught
    up to the speed of the serial implementation.

    However, Intel processors provide a CPU instruction called @pshufb@ that is
    equivalent to `mappend` for `Transition`.  This means that we can:

    * chunk up the input `ByteString`s into N smaller `ByteString`s (where N is
      typically the number of processors)
    * process each smaller `ByteString` in parallel using an efficient `mconcat`
      written in C using the @pshufb@ instruction

    This means that you should only expect good performance on Intel processors.
    Additionally, this library uses @gcc@ intrinsics to emit the above
    instruction so this package will not compile by default on OS X (which uses
    @clang@).

    This library only supports state machines of up to 16 states, but the
    original paper describes how to support larger state machines using a
    @blend@ instruction.  I'm not sure how to get @gcc@ to emit the @blend@
    instruction, so this package is limited to 16 states for now.
-}

foreign import ccall "run" c_run
    :: Ptr CChar -> CSize -> Ptr CChar -> Ptr CChar -> IO ()

{-| Run a `StateMachine` on a `ByteString`

    `run` returns a `Transition` representing what each final state would be for
    every possible initial state

    The implementation is equivalent to:

prop> run (StateMachine f) bytes == foldMap f (Data.ByteString.unpack bytes)

    ... except much more efficient
-}
run :: StateMachine -> ByteString -> Transition
run matrix bytes = Data.Binary.decode (Data.ByteString.Lazy.fromStrict (
    Foreign.Marshal.Unsafe.unsafeLocalState (do
        Data.ByteString.Unsafe.unsafeUseAsCStringLen tBytes (\(ptrTBytes, _) ->
            Data.ByteString.Unsafe.unsafeUseAsCStringLen bytes (\(ptrIn, len) ->
                Foreign.allocaBytes 16 (\ptrOut -> do
                    c_run ptrIn (fromIntegral len) ptrTBytes ptrOut
                    Data.ByteString.packCStringLen (ptrOut, 16) ) ) ) ) ))
  where
    tBytes = Data.ByteString.Lazy.toStrict (Data.Binary.encode matrix)

-- | Split a `ByteString` into chunks of size @n@
chunkBytes :: Int -> ByteString -> [ByteString]
chunkBytes n bytes =
    if Data.ByteString.null bytes
    then []
    else prefix : chunkBytes n suffix
  where
    ~(prefix, suffix) = Data.ByteString.splitAt n bytes

{-| `runInParallel` is the same as `run` except in parallel

    The first argument specifies how many threads to use to accelerate the
    computation.  A good rule of thumb is to use the number of cores your
    machine has, like this:

    > ...
    > numCores <- Control.Concurrent.getNumCapabilities
    > let transition = runInParallel numCores stateMachine bytes
    > ...

    `runInParallel` is \"embarassingly parallel\", meaning that the performance
    scales linearly with the number of available cores

prop> runInParallel n (StateMachine f) bytes == foldMap f (Data.ByteString.unpack bytes)
-}
runInParallel :: Int -> StateMachine -> ByteString -> Transition
runInParallel numThreads matrix bytes =
    mconcat
        (Control.Parallel.Strategies.parMap
            Control.Parallel.Strategies.rseq
            (run matrix)
            (chunkBytes subLen bytes) )
  where
    len = Data.ByteString.length bytes

    subLen = ((len - 1) `div` numThreads) + 1
