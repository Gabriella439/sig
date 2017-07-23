{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

-- | This module provides the `State` type

module Sig.State
    ( -- * State
      State(..)
    ) where

import Data.Binary (Binary(..))
import Data.Word (Word8)
import Dhall (Interpret)
import GHC.Generics (Generic)

{-| This library supports state machines with up to 16 states

    The `Sig.Transition.Transition` type uses this type to specify the final
    state that each initial state transitions to.  For example, a
    `Sig.Transition.Transition` type with the following field:

> Transition
>     { fromState00To = S03
>     ...

    ... specifies that if the state machine is currently in State @#0@ then the
    state machine should transition to state @3@.

    You can create a `State` by:

    * using the constructors (i.e. `S00`, `S01`, etc.)
    * decoding a `State` from a `Data.ByteString.Lazy.ByteString` using the
      `Binary` instance
    * decoding a `State` from a Dhall expression using the `Interpret` instance

    You can consume a `State`:

    * indirectly, using `Sig.run` and `Sig.runInParallel`
    * directly, by encoding to a `Data.ByteString.Lazy.ByteString` using the
      `Binary` instance
-}
data State
    = S00
    | S01
    | S02
    | S03
    | S04
    | S05
    | S06
    | S07
    | S08
    | S09
    | S10
    | S11
    | S12
    | S13
    | S14
    | S15
    deriving (Generic, Interpret, Show)

instance Binary State where
    put S00 = put ( 0 :: Word8)
    put S01 = put ( 1 :: Word8)
    put S02 = put ( 2 :: Word8)
    put S03 = put ( 3 :: Word8)
    put S04 = put ( 4 :: Word8)
    put S05 = put ( 5 :: Word8)
    put S06 = put ( 6 :: Word8)
    put S07 = put ( 7 :: Word8)
    put S08 = put ( 8 :: Word8)
    put S09 = put ( 9 :: Word8)
    put S10 = put (10 :: Word8)
    put S11 = put (11 :: Word8)
    put S12 = put (12 :: Word8)
    put S13 = put (13 :: Word8)
    put S14 = put (14 :: Word8)
    put S15 = put (15 :: Word8)

    get = do
        n <- get
        case n :: Word8 of
            0  -> return S00
            1  -> return S01
            2  -> return S02
            3  -> return S03
            4  -> return S04
            5  -> return S05
            6  -> return S06
            7  -> return S07
            8  -> return S08
            9  -> return S09
            10 -> return S10
            11 -> return S11
            12 -> return S12
            13 -> return S13
            14 -> return S14
            15 -> return S15
            _  -> fail ("Data.Binary.get[State]: Unexpected Word8: " ++ show n)
