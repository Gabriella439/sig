{-# LANGUAGE BangPatterns #-}

-- | This module provides the `Transition` type

module Sig.Transition
    ( -- * Transition
      Transition(..)
    ) where

import Data.Binary (Binary(..))
import Data.Vector ((!))
import Sig.State (State(..))

import qualified Data.Vector

-- | A `Transition` is a function from a `State` to another `State`
newtype Transition = Transition { runTransition :: State -> State }

instance Monoid Transition where
    mempty = Transition id

    mappend (Transition f) (Transition g) = Transition (g . f)

instance Binary Transition where
    put (Transition f) = mapM_ (put . f) [minBound..maxBound]

    get = do
        let numStates = fromEnum (maxBound :: State) + 1
        !ss <- Data.Vector.replicateM numStates get
        return (Transition (\s -> ss ! fromEnum s))
