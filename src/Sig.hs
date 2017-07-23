module Sig
    ( run
    , runInParallel
    , StateMachine(..)
    , buildStateMachine
    , Transition(..)
    , buildTransition
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

foreign import ccall "run" c_run
    :: Ptr CChar -> CSize -> Ptr CChar -> Ptr CChar -> IO ()

{-| Efficiently run a `StateMachine` on a `ByteString`

    `run` returns a `Transition` representing what each final state would be for
    every possible initial state

    Under the hood `run` uses an C implementation that takes advantage of the
    @pshufb@ instruction to efficiently simulate 16 states at a time
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

-- | Split the `ByteString` into @k@ chunks and call `run` in parallel
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
