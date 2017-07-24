{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}

-- | This module provides the `Transition` type

module Sig.Transition
    ( -- * Transition
      Transition(..)
    , buildTransition
    ) where

import Data.Binary (Binary(..))
import Dhall (Interpret)
import GHC.Generics (Generic)
import Sig.State (State(..))

{-| The `Transition` type encodes one step of the state machine by specifying
    the next state that each initial state transitions to.  For example, the
    following transition:

> Transition
>     { fromState00To = S03
>     , fromState01To = S02
>     ...
>     }

    ... specifies that if the state machine is currently in State @#0@ then the
    state machine should transition to state @3@ or if the state machine is
    currently in State @#1@ then the state machine should transition to state
    @2@

    You can create a `Transition` by:

    * using the `Transition` constructor
    * using the `buildTransition` function
    * decoding a `Transition` from a `Data.ByteString.Lazy.ByteString` using the
      `Binary` instance
    * decoding a `Transition` from a Dhall expression using the `Interpret`
      instance
    * using `mempty`, which represents the `Transition` that does nothing

    You can combine `Transition`s by:

    * using @`mappend` x y@, which represents the transition @x@ followed by the
      transition @y@

    You can consume `Transition`s:

    * indirectly, using `Sig.run` and `Sig.runInParallel`
    * directly, by encoding a `Transition` to a
      `Data.ByteString.Lazy.ByteString` using the `Binary` instance
-}

data Transition = Transition
    { fromState00To :: !State
    , fromState01To :: !State
    , fromState02To :: !State
    , fromState03To :: !State
    , fromState04To :: !State
    , fromState05To :: !State
    , fromState06To :: !State
    , fromState07To :: !State
    , fromState08To :: !State
    , fromState09To :: !State
    , fromState10To :: !State
    , fromState11To :: !State
    , fromState12To :: !State
    , fromState13To :: !State
    , fromState14To :: !State
    , fromState15To :: !State
    } deriving (Generic, Interpret, Show)

instance Monoid Transition where
    mempty = Transition {..}
      where
        fromState00To = S00
        fromState01To = S01
        fromState02To = S02
        fromState03To = S03
        fromState04To = S04
        fromState05To = S05
        fromState06To = S06
        fromState07To = S07
        fromState08To = S08
        fromState09To = S09
        fromState10To = S10
        fromState11To = S11
        fromState12To = S12
        fromState13To = S13
        fromState14To = S14
        fromState15To = S15

    mappend transitionL transitionR = Transition
        { fromState00To = go fromState00To
        , fromState01To = go fromState01To
        , fromState02To = go fromState02To
        , fromState03To = go fromState03To
        , fromState04To = go fromState04To
        , fromState05To = go fromState05To
        , fromState06To = go fromState06To
        , fromState07To = go fromState07To
        , fromState08To = go fromState08To
        , fromState09To = go fromState09To
        , fromState10To = go fromState10To
        , fromState11To = go fromState11To
        , fromState12To = go fromState12To
        , fromState13To = go fromState13To
        , fromState14To = go fromState14To
        , fromState15To = go fromState15To
        }
      where
        go f = toAccessor (f transitionL) transitionR

        toAccessor S00 = fromState00To
        toAccessor S01 = fromState01To
        toAccessor S02 = fromState02To
        toAccessor S03 = fromState03To
        toAccessor S04 = fromState04To
        toAccessor S05 = fromState05To
        toAccessor S06 = fromState06To
        toAccessor S07 = fromState07To
        toAccessor S08 = fromState08To
        toAccessor S09 = fromState09To
        toAccessor S10 = fromState10To
        toAccessor S11 = fromState11To
        toAccessor S12 = fromState12To
        toAccessor S13 = fromState13To
        toAccessor S14 = fromState14To
        toAccessor S15 = fromState15To

instance Binary Transition where
    put (Transition {..}) = do
        put fromState00To
        put fromState01To
        put fromState02To
        put fromState03To
        put fromState04To
        put fromState05To
        put fromState06To
        put fromState07To
        put fromState08To
        put fromState09To
        put fromState10To
        put fromState11To
        put fromState12To
        put fromState13To
        put fromState14To
        put fromState15To

    get = do
        fromState00To <- get
        fromState01To <- get
        fromState02To <- get
        fromState03To <- get
        fromState04To <- get
        fromState05To <- get
        fromState06To <- get
        fromState07To <- get
        fromState08To <- get
        fromState09To <- get
        fromState10To <- get
        fromState11To <- get
        fromState12To <- get
        fromState13To <- get
        fromState14To <- get
        fromState15To <- get
        return (Transition {..})

{-| Build a `Transition` from a function

    This comes in handy when you want to use Haskell's support for wildcard
    pattern matches to avoid having to specify every `State`

    For example:

> buildTransition (\state -> case state of
>     S00 -> S01
>     S01 -> S02
>     S02 -> S02
>     _   -> S00 )
-}
buildTransition :: (State -> State) -> Transition
buildTransition f = Transition {..}
  where
    fromState00To = f S00
    fromState01To = f S01
    fromState02To = f S02
    fromState03To = f S03
    fromState04To = f S04
    fromState05To = f S05
    fromState06To = f S06
    fromState07To = f S07
    fromState08To = f S08
    fromState09To = f S09
    fromState10To = f S10
    fromState11To = f S11
    fromState12To = f S12
    fromState13To = f S13
    fromState14To = f S14
    fromState15To = f S15
