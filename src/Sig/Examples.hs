{-# LANGUAGE RecordWildCards #-}

module Sig.Examples where

import Sig (State(..), StateMachine)

import qualified Sig

-- | `StateMachine` for matching C-style comments
cStyleComments :: StateMachine
cStyleComments = Sig.buildStateMachine f
  where
    f 42 S00 = S00
    f 42 S01 = S02
    f 42 S02 = S03
    f 42 S03 = S03

    f 47 S00 = S01
    f 47 S01 = S01
    f 47 S02 = S02
    f 47 S03 = S00

    f  _ S00 = S00
    f  _ S01 = S00
    f  _ S02 = S02
    f  _ S03 = S02

{-| `StateMachine` that tests for the presence of an ASCII-encoded
    @"Hello, world!"@ anywhere within the `Data.ByteString.ByteString`
-}
helloWorld :: StateMachine
helloWorld = Sig.buildStateMachine f
  where
    f  _  S13 = S13

    -- @'H'@
    f  72 S00 = S01
    f  72 _   = S00

    -- @'e'@
    f 101 S01 = S02
    f 101 _   = S00

    -- @'l'@
    f 108 S02 = S03
    f 108 S03 = S04
    f 108 S10 = S11
    f 108 _   = S00

    -- @'o'@
    f 111 S04 = S05
    f 111 S08 = S09
    f 111 _   = S00

    -- @','@
    f  44 S05 = S06
    f  44 _   = S00

    -- @' '@
    f  32 S06 = S07
    f  32 _   = S00

    -- @'w'@
    f 119 S07 = S08
    f 119 _   = S00

    -- @'r'@
    f 114 S09 = S10
    f 114 _   = S00

    -- @'d'@
    f 100 S11 = S12
    f 100 _   = S00

    -- @'!'@
    f  33 S12 = S13
    f  33 _   = S00

    f  _  _   = S00
