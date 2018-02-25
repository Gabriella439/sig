module Sig.Examples where

import Sig (State(..), StateMachine)

import qualified Sig

{-| `StateMachine` for matching well-formed C-style comments

    The match succeeds if the state `S00` transitions to state `S00`
-}
cStyleComments :: StateMachine
cStyleComments = Sig.buildStateMachine f
  where
    -- '*'
    f 42 S00 = S00
    f 42 S01 = S02
    f 42 S02 = S03
    f 42 S03 = S03

    -- '/'
    f 47 S00 = S01
    f 47 S01 = S01
    f 47 S02 = S02
    f 47 S03 = S00

    f  _ S00 = S00
    f  _ S01 = S00
    f  _ S02 = S02
    f  _ S03 = S02

    f  _ _   = S00

{-| `StateMachine` that tests for the presence of an UTF8-encoded
    @"Hello, world!"@

    The match succeeds if state `S00` transitions to state `S13`
-}
helloWorld :: StateMachine
helloWorld = Sig.buildStateMachine f
  where
    -- Acceptor state
    f  _  S13 = S13

    -- 'H'
    f  72 S00 = S01

    -- 'e'
    f 101 S01 = S02

    -- 'l'
    f 108 S02 = S03

    -- 'l'
    f 108 S03 = S04

    -- 'o'
    f 111 S04 = S05

    -- ','
    f  44 S05 = S06

    -- ' '
    f  32 S06 = S07

    -- 'w'
    f 119 S07 = S08

    -- 'o'
    f 111 S08 = S09

    -- 'r'
    f 114 S09 = S10

    -- 'l'
    f 108 S10 = S11

    -- 'd'
    f 100 S11 = S12

    -- '!'
    f  33 S12 = S13

    f  _  _   = S00

{-| `StateMachine` that searches for UTF8-encoded @module .*where@ to guess if a
    file is a Haskell module

    The match succeeds if state `S00` transitions to state `S12`
-}
haskellModule :: StateMachine
haskellModule = Sig.buildStateMachine f
  where
    -- Acceptor state
    f _   S12 = S12

    -- 'm'
    f 109 _   = S01

    -- 'o'
    f 111 S01 = S02

    -- 'd'
    f 100 S02 = S03

    -- 'u'
    f 117 S03 = S04

    -- 'l'
    f 108 S04 = S05

    -- 'e'
    f 101 S05 = S06

    -- ' '
    f  32 S06 = S07

    -- '.*w'
    f 119 S07 = S08
    f _   S07 = S07

    -- 'h'
    f 104 S08 = S09

    -- 'e'
    f 101 S09 = S10

    -- 'r'
    f 114 S10 = S11

    -- 'e'
    f 101 S11 = S12

    f _   _   = S00
