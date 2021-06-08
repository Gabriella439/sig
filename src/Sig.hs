{-# LANGUAGE BangPatterns   #-}

{-| This library provides a very efficient implementation of parallel state
    machines running over `ByteString` inputs based on the following paper:

    > Mytkowicz, Todd, Madanlal Musuvathi, and Wolfram Schulte. "Data-parallel
    > finite-state machines." ACM SIGARCH Computer Architecture News. Vol. 42.
    > No. 1. ACM, 2014.

    These state machines are \"parallel\" in two senses of the word:

    * The machine simulates multiple states in parallel
    * You can process the input `ByteString` itself in parallel

    This state machine implementation gives excellent performance which also
    scales linearly with the number of available cores

    The main limitations of this library are that:

    * the state machines are currently limited to 64 states

      The original paper describes supporting 256 states, but this package only
      goes up to 64 states in order to support all of the optimizations from the
      paper while keeping the C code manageable.

    * this package has to be built using @gcc@ as the C compiler and can only
      be run on architectures that supports @-mssse3@, @-msse4.2@, and @-mavx2@.

      Most processors built since 2012 support these SIMD extensions. See:

      * <https://en.wikipedia.org/wiki/SSSE3#CPUs_with_SSSE3 CPUS that support SSSE3>
      * <https://en.wikipedia.org/wiki/SSE4#Supporting_CPUs CPUS that support SSE4>
      * <https://en.wikipedia.org/wiki/Advanced_Vector_Extensions#CPUs_with_AVX CPUS that support AVX>
-}

module Sig
    ( -- * Example
      -- $example

      -- * Building state machines
      buildStateMachine
    , State
    , maxStates
    , Transition(..)
    , StateMachine(..)

      -- * Running state machines
    , run

    ) where

import Data.Binary (Binary(..))
import Data.ByteString (ByteString)
import Data.Vector ((!))
import Data.Word (Word8)
import Foreign (Ptr)
import Foreign.C.Types (CChar(..), CSize(..))

import qualified Control.Parallel.Strategies
import qualified Data.Binary                 as Binary
import qualified Data.ByteString             as ByteString
import qualified Data.ByteString.Lazy        as ByteString.Lazy
import qualified Data.ByteString.Unsafe      as ByteString.Unsafe
import qualified Data.Vector                 as Vector
import qualified Foreign
import qualified Foreign.Marshal.Unsafe

-- $example
-- 
-- Here is an example of how you would define a `StateMachine` that parses
-- C-style block comments using four states (which you can find in the
-- "Sig.Examples" module):
--
-- * @0@ - Starting state and final state for a well-formed comment
-- * @1@ - Just parsed a @\'/\'@ that might be the first character in @\"/*\"@
-- * @2@ - In the middle of a block comment
-- * @3@ - Just parsed a @\'*\'@ that might be the first character in @\"*/\"@
--
-- > import Sig (State(..), StateMachine)
-- > 
-- > import qualified Sig
-- > 
-- > cStyleComments :: StateMachine
-- > cStyleComments = Sig.buildStateMachine f
-- >   where
-- >     -- 47 is the ASCII encoding for '/'
-- >     -- 42 is the ASCII encoding for '*'
-- >
-- >     f 47 0 = 1  -- Possible  comment start: Go to state #1
-- >     f 42 1 = 2  -- Confirmed comment start: Go to state #2
-- >     f 42 2 = 3  -- Possible  comment end  : Go to state #3
-- >     f 47 3 = 0  -- Confirmed comment end  : Go to state #0
-- >
-- >     f 47 1 = 1  -- Still might be a comment start: Stay on   state #1
-- >     f  _ 1 = 0  -- Not a comment after all       : Return to state #0
-- >
-- >     f 42 3 = 3  -- Still might be a comment end  : Stay on   state #3
-- >     f  _ 3 = 2  -- Not a comment after all       : Return to state #2
-- >
-- >     f  _ 0 = 0  -- Outside of a comment: Stay on state #0
-- >
-- >     f  _ 2 = 2  -- Inside a comment    : Stay on state #2
-- >
-- >     f  _ _ = 0
--
-- ... and here is an example of using the above `StateMachine` on a file:
--
-- > module Sig.Main where
-- >
-- > import Sig (State(..), Transition(..))
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
-- >     let transition = Sig.run n Sig.Examples.cStyleComments bytes
-- >     print (runTransition transition 0 == 0)

{-| This library supports only state machines with up to 64 states (and may
    support more in the future)

    Rather than modeling the `State` as an enum with 64 alternatives we use an
    `Word8` for simplicity.  States greater than 64 are ignored
-}
type State = Word8

-- | The maximum number of states this package supports
maxStates :: Word8
maxStates = 64

-- | A `Transition` is a function from a `State` to another `State`
newtype Transition = Transition { runTransition :: State -> State }

instance Semigroup Transition where
    Transition f <> Transition g = Transition (g . f)

instance Monoid Transition where
    mempty = Transition id

instance Binary Transition where
    put (Transition f) = mapM_ (put . f) [0..(maxStates - 1)]

    get = do
        !ss <- Vector.replicateM (fromIntegral maxStates) get
        return (Transition (\s -> ss ! fromEnum s))

instance Eq Transition where
    a == b = Binary.encode a == Binary.encode b

instance Show Transition where
    showsPrec _ transition =
        ("(Data.Binary.decode (Data.ByteString.Lazy.pack " <>) . showsPrec 9 (ByteString.Lazy.unpack (Binary.encode transition)) . (") :: Sig.Transition)" <>)

-- | A `StateMachine` is a function from a byte (i.e. `Word8`) to a `Transition`
newtype StateMachine = StateMachine { runStateMachine :: Word8 -> Transition }

instance Binary StateMachine where
    put (StateMachine k) = mapM_ (put . k) [minBound..maxBound]

    get = do
        let numBytes = fromEnum (maxBound :: Word8) + 1
        ts <- Vector.replicateM numBytes get
        return (StateMachine (\word8 -> ts ! fromEnum word8))

instance Eq StateMachine where
    a == b = Binary.encode a == Binary.encode b

instance Show StateMachine where
    showsPrec _ stateMachine =
        ("(Data.Binary.decode (Data.ByteString.Lazy.pack " <>) . showsPrec 9 (ByteString.Lazy.unpack (Binary.encode stateMachine)) . (") :: Sig.StateMachine)" <>)

{-| Convenient utility to build a `StateMachine` from a function of two
    arguments
-}
buildStateMachine :: (Word8 -> State -> State) -> StateMachine
buildStateMachine f = StateMachine (fmap Transition f)

foreign import ccall "run" c_run
    :: Ptr CChar -> CSize -> Ptr CChar -> Ptr CChar -> IO ()

{-| Wrap the @c_run@ function in a Haskell API

    prop> run 1 stateMachine bytes === foldMap (runStateMachine stateMachine) (ByteString.unpack bytes)
-}
runSerial :: StateMachine -> ByteString -> Transition
runSerial matrix bytes = Binary.decode (ByteString.Lazy.fromStrict (
    Foreign.Marshal.Unsafe.unsafeLocalState (do
        ByteString.Unsafe.unsafeUseAsCStringLen tBytes (\(ptrTBytes, _) ->
            ByteString.Unsafe.unsafeUseAsCStringLen bytes (\(ptrIn, len) ->
                Foreign.allocaBytes (fromIntegral maxStates) (\ptrOut -> do
                    c_run ptrIn (fromIntegral len) ptrTBytes ptrOut
                    ByteString.packCStringLen (ptrOut, (fromIntegral maxStates)) ) ) ) ) ))
  where
    tBytes = ByteString.Lazy.toStrict (Binary.encode matrix)

-- | Split a `ByteString` into chunks of size @n@
chunkBytes :: Int -> ByteString -> [ByteString]
chunkBytes n bytes =
    if ByteString.null bytes
    then []
    else prefix : chunkBytes n suffix
  where
    ~(prefix, suffix) = ByteString.splitAt n bytes

{-| Run a `StateMachine` on a `ByteString`

    `run` returns a `Transition` that computes what the final state would be for
    each possible initial state

    The implementation is equivalent to:

    prop> run n (StateMachine f) bytes == foldMap f (ByteString.unpack bytes)

    ... except much more efficient and parallel

    The first argument specifies how many threads to use to accelerate the
    computation.  A good rule of thumb is to use the number of cores your
    machine has, like this:

    > ...
    > numCores <- Control.Concurrent.getNumCapabilities
    > let transition = run numCores stateMachine bytes
    > ...

    ... or you can just specify @1@ thread for a serial implementation (which
    will still be really efficient)

    `run` is \"embarassingly parallel\", meaning that the performance scales
    linearly with the number of available cores
-}
run :: Int
    -- ^ Number of threads to use
    -> StateMachine
    -- ^ State machine to run over the input bytes
    -> ByteString
    -- ^ Input bytes to feed to the state machine
    -> Transition
    -- ^ Computed function from every starting state to every final state
run 1          matrix bytes = runSerial matrix bytes
run numThreads matrix bytes =
    mconcat
        (Control.Parallel.Strategies.parMap
            Control.Parallel.Strategies.rseq
            (runSerial matrix)
            (chunkBytes subLen bytes) )
  where
    len = ByteString.length bytes

    subLen = ((len - 1) `div` numThreads) + 1
