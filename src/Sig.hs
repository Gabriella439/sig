{-# LANGUAGE OverloadedStrings #-}

module Sig where

import Data.ByteString (ByteString)
import Foreign (Ptr)
import Foreign.C.Types

import qualified Control.Concurrent
import qualified Control.Parallel.Strategies
import qualified Data.ByteString
import qualified Data.ByteString.Unsafe
import qualified Foreign
import qualified Foreign.Marshal.Unsafe
import qualified System.IO.MMap

foreign import ccall "process" c_process
    :: Ptr CChar -> CSize -> Ptr CChar -> Ptr CChar -> IO ()

-- Wrap C `process` in a pure interface
process :: ByteString -> ByteString -> ByteString
process tBytes bytes =
    Foreign.Marshal.Unsafe.unsafeLocalState (do
        Data.ByteString.Unsafe.unsafeUseAsCStringLen tBytes (\(ptrTBytes, _) ->
            Data.ByteString.Unsafe.unsafeUseAsCStringLen bytes (\(ptrIn, len) ->
                Foreign.allocaBytes 16 (\ptrOut -> do
                    c_process ptrIn (fromIntegral len) ptrTBytes ptrOut
                    Data.ByteString.packCStringLen (ptrOut, 16) ) ) ) )

-- Example transition matrix from paper for matching C-style comments
tBytes :: ByteString
tBytes =
    Data.ByteString.pack
        (concat
            (   replicate 42 def
            ++  [t42]
            ++  replicate 4 def
            ++  [t47]
            ++  replicate 208 def
            )
        )
  where
    def = [0, 0, 2, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
    t42 = [0, 2, 3, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
    t47 = [1, 1, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]

-- | Split a `ByteString` into chunks of size @n@
chunkBytes :: Int -> ByteString -> [ByteString]
chunkBytes n bytes =
    if Data.ByteString.null bytes
    then []
    else prefix : chunkBytes n suffix
  where
    ~(prefix, suffix) = Data.ByteString.splitAt n bytes

-- Split the `ByteString` into @k@ chunks and call `process` in parallel
parallelProcess :: Int -> ByteString -> [ByteString]
parallelProcess k bytes =
    Control.Parallel.Strategies.parMap
        Control.Parallel.Strategies.rseq
        (process tBytes)
        (chunkBytes (len `div` k) bytes)
  where
    len = Data.ByteString.length bytes

main :: IO ()
main = do
    k     <- Control.Concurrent.getNumCapabilities
    bytes <- System.IO.MMap.mmapFileByteString "test.txt" Nothing
    -- Not yet implemented, combine the `k` transition matrices into the final
    -- transition matrix.  This is cheap since `k` is the number of cores, I
    -- just haven't had a chance to complete this yet.
    print (parallelProcess k bytes)
