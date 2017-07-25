{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | This module provides the `StateMachine` type

module Sig.StateMachine
    ( -- * StateMachine
      StateMachine(..)
    , buildStateMachine
    ) where

import Data.Binary (Binary(..))
import Data.Word (Word8)
import Data.Vector ((!))
import Sig.State (State)
import Sig.Transition (Transition(..))

import qualified Data.Vector

{-| A `StateMachine` is a function from a byte (i.e. `Word8`) to a `Transition`

    You can create a `StateMachine` by:

    * using the `StateMachine` constructor to build a `StateMachine` from a
      function
    * using the `buildStateMachine` utility function
    * decoding a `StateMachine` from a `Data.ByteString.Lazy.ByteString` using
      the `Binary` instance
    * using `mempty`, which represents a `StateMachine` that does nothing

    You can combine `StateMachine`s by:

    * using @`mappend` x y@, which runs the `StateMachine` @x@ and then the
      `StateMachine` @y@ for each byte

    You can consume `StateMachine`s by:

    * using `Sig.run` and `Sig.runInParallel`
    * using `runStateMachine` to access the underlying function
    * encoding a `StateMachine` to a `Data.ByteString.Lazy.ByteString` using the
      `Binary` instance
-}
newtype StateMachine = StateMachine { runStateMachine :: Word8 -> Transition }
    deriving (Monoid)

instance Binary StateMachine where
    put (StateMachine k) = mapM_ (put . k) [minBound..maxBound]

    get = do
        let numBytes = fromEnum (maxBound :: Word8) + 1
        ts <- Data.Vector.replicateM numBytes get
        return (StateMachine (\word8 -> ts ! fromEnum word8))

{-| Convenient utility to build a `StateMachine` from a function of two
    arguments

    Example usage:

> cStyleComments :: StateMachine
> cStyleComments = Sig.buildStateMachine f
>   where
>     f 42 S00 = S00
>     f 42 S01 = S02
>     f 42 S02 = S03
>     f 42 S03 = S03
> 
>     f 47 S00 = S01
>     f 47 S01 = S01
>     f 47 S02 = S02
>     f 47 S03 = S00
> 
>     f  _ S00 = S00
>     f  _ S01 = S00
>     f  _ S02 = S02
>     f  _ S03 = S02
>
>     f  _ _   = S00
-}
buildStateMachine :: (Word8 -> State -> State) -> StateMachine
buildStateMachine f = StateMachine (fmap Transition f)
