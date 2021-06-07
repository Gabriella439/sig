{-| This module provides some sample hand-rolle state machines that you can
    use for testing purposes
-}
module Sig.Examples where

import Sig (StateMachine)

import qualified Sig

{-| `StateMachine` for matching well-formed C-style comments

    The match succeeds if the state @0@ transitions to state @0@
-}
cStyleComments :: StateMachine
cStyleComments = Sig.buildStateMachine f
  where
    -- 47 is the ASCII encoding for '/'
    -- 42 is the ASCII encoding for '*'

    f 47 0 = 1  -- Possible  comment start: Go to state #1
    f 42 1 = 2  -- Confirmed comment start: Go to state #2
    f 42 2 = 3  -- Possible  comment end  : Go to state #3
    f 47 3 = 0  -- Confirmed comment end  : Go to state #0

    f 47 1 = 1  -- Still might be a comment start: Stay on   state #1
    f  _ 1 = 0  -- Not a comment after all       : Return to state #0

    f 42 3 = 3  -- Still might be a comment end  : Stay on   state #3
    f  _ 3 = 2  -- Not a comment after all       : Return to state #2

    f  _ 0 = 0  -- Outside of a comment: Stay on state #0

    f  _ 2 = 2  -- Inside a comment    : Stay on state #2

    f  _ _ = 0

{-| `StateMachine` that tests for the presence of a UTF8-encoded
    @"Hello, world!"@

    The match succeeds if state @0@ transitions to state @13@
-}
helloWorld :: StateMachine
helloWorld = Sig.buildStateMachine f
  where
    -- Acceptor state
    f  _  13 = 13

    -- 'H'
    f  72  0 =  1

    -- 'e'
    f 101  1 =  2

    -- 'l'
    f 108  2 =  3

    -- 'l'
    f 108  3 =  4

    -- 'o'
    f 111  4 =  5

    -- ','
    f  44  5 =  6

    -- ' '
    f  32  6 =  7

    -- 'w'
    f 119  7 =  8

    -- 'o'
    f 111  8 =  9

    -- 'r'
    f 114  9 = 10

    -- 'l'
    f 108 10 = 11

    -- 'd'
    f 100 11 = 12

    -- '!'
    f  33 12 = 13

    f   _  _ =  0

{-| `StateMachine` that searches for UTF8-encoded @module .*where@ to guess if a
    file is a Haskell module

    The match succeeds if state @0@ transitions to state @12@
-}
haskellModule :: StateMachine
haskellModule = Sig.buildStateMachine f
  where
    -- Acceptor state
    f _   12 = 12

    -- 'm'
    f 109  _ =  1

    -- 'o'
    f 111  1 =  2

    -- 'd'
    f 100  2 =  3

    -- 'u'
    f 117  3 =  4

    -- 'l'
    f 108  4 =  5

    -- 'e'
    f 101  5 =  6

    -- ' '
    f  32  6 =  7

    -- '.*w'
    f 119  7 =  8
    f _    7 =  7

    -- 'h'
    f 104  8 =  9

    -- 'e'
    f 101  9 = 10

    -- 'r'
    f 114 10 = 11

    -- 'e'
    f 101 11 = 12

    f _    _ =  0
