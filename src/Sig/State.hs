{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

-- | This module provides the `State` type

module Sig.State
    ( -- * State
      State(..)
    ) where

import Data.Binary (Binary)
import GHC.Generics (Generic)

{-| This library supports state machines with up to 16 states (and may support
    more in the future)

    This type represents the set of possible states that the state machine can
    be in
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
    deriving (Binary, Bounded, Enum, Eq, Generic, Ord, Show)
