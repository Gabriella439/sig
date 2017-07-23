module Sig
    ( module Sig
    , module Sig.Matrix
    , module Sig.State
    , module Sig.Transition
    ) where

import Data.ByteString (ByteString)
import Foreign (Ptr)
import Foreign.C.Types (CChar(..), CSize(..))
import Sig.Matrix
import Sig.State
import Sig.Transition

import qualified Control.Parallel.Strategies
import qualified Data.Binary
import qualified Data.ByteString
import qualified Data.ByteString.Lazy
import qualified Data.ByteString.Unsafe
import qualified Foreign
import qualified Foreign.Marshal.Unsafe

foreign import ccall "process" c_process
    :: Ptr CChar -> CSize -> Ptr CChar -> Ptr CChar -> IO ()

-- Wrap C `process` in a pure interface
process :: Matrix -> ByteString -> Transition
process matrix bytes = Data.Binary.decode (Data.ByteString.Lazy.fromStrict (
    Foreign.Marshal.Unsafe.unsafeLocalState (do
        Data.ByteString.Unsafe.unsafeUseAsCStringLen tBytes (\(ptrTBytes, _) ->
            Data.ByteString.Unsafe.unsafeUseAsCStringLen bytes (\(ptrIn, len) ->
                Foreign.allocaBytes 16 (\ptrOut -> do
                    c_process ptrIn (fromIntegral len) ptrTBytes ptrOut
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

-- Split the `ByteString` into @k@ chunks and call `process` in parallel
parallelProcess :: Matrix -> Int -> ByteString -> Transition
parallelProcess matrix k bytes =
    mconcat
        (Control.Parallel.Strategies.parMap
            Control.Parallel.Strategies.rdeepseq
            (process matrix)
            (chunkBytes (len `div` k) bytes) )
  where
    len = Data.ByteString.length bytes
