{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

{-# OPTIONS_GHC -Wno-missing-signatures -Wno-orphans #-}

module Driver where

import Data.ByteString (ByteString)
import Data.Word (Word8)
import Sig (State, StateMachine(..), Transition(..))
import Sig.Examples (cStyleComments, helloWorld, haskellModule)
import Test.Tasty.QuickCheck (Arbitrary(..), Gen, (===))

import qualified Control.Monad         as Monad
import qualified Data.Binary           as Binary
import qualified Data.ByteString       as ByteString
import qualified Data.ByteString.Char8 as ByteString.Char8
import qualified Data.ByteString.Lazy  as ByteString.Lazy
import qualified Sig
import qualified Test.Tasty.HUnit      as HUnit
import qualified Test.Tasty.QuickCheck as QuickCheck

state :: Gen Word8
state = QuickCheck.suchThat arbitrary (< Sig.maxStates)

instance Arbitrary ByteString where
    arbitrary = fmap ByteString.pack arbitrary

    shrink = QuickCheck.shrinkMap ByteString.pack ByteString.unpack

instance Arbitrary Transition where
    arbitrary = do
        bytes <- Monad.replicateM (fromIntegral Sig.maxStates) state
        return (Binary.decode (ByteString.Lazy.pack bytes))

instance Arbitrary StateMachine where
    arbitrary = do
        bytes <- Monad.replicateM (fromIntegral Sig.maxStates * 256) state
        return (Binary.decode (ByteString.Lazy.pack bytes))

test :: StateMachine -> Int -> ByteString -> State
test stateMachine threads input =
    Sig.runTransition (Sig.run threads stateMachine input) 0

spaces :: ByteString
spaces = ByteString.Char8.replicate 1000 ' '

unit_comments_0 = HUnit.assertEqual "" 0 (test cStyleComments 1 "")

unit_comments_1 = HUnit.assertEqual "" 1 (test cStyleComments 1 "/")

unit_comments_2 = HUnit.assertEqual "" 2 (test cStyleComments 1 "/*")

unit_comments_3 = HUnit.assertEqual "" 3 (test cStyleComments 1 "/**")

unit_comments_4 = HUnit.assertEqual "" 0 (test cStyleComments 1 "/**/")

unit_comments_5 = HUnit.assertEqual "" 1 (test cStyleComments 1 "/* */ */")

unit_comments_6 = HUnit.assertEqual "" 0 (test cStyleComments 1 "/* /* */")

unit_comments_7 =
    HUnit.assertEqual "" 0 (test cStyleComments 4 ("/*" <> spaces <> "*/"))

unit_hello_0 = HUnit.assertEqual "" 13 (test helloWorld 1 "Hello, world!")

unit_hello_1 =
    HUnit.assertEqual "" 13
        (test helloWorld 4 (spaces <> "Hello, world!" <> spaces))

unit_haskell_0 =
    HUnit.assertEqual "" 12 (test haskellModule 1 "where modulmodule whermodule where")

unit_haskell_1 =
    HUnit.assertEqual "" 11 (test haskellModule 1 "where modulodule wheremodule wher")

unit_haskell_2 =
    HUnit.assertEqual "" 12
        (test haskellModule 4
            (spaces <> "module" <> spaces <> "where" <> spaces)
        )

prop_roundtrip stateMachine =
        Binary.encode @StateMachine (Binary.decode (Binary.encode @StateMachine stateMachine))
    === Binary.encode @StateMachine stateMachine

prop_foldMap_serial stateMachine bytes =
        Sig.run 1 stateMachine bytes
    === foldMap (Sig.runStateMachine stateMachine) (ByteString.unpack bytes)

prop_foldMap_parallel stateMachine bytes =
        Sig.run 4 stateMachine bytes
    === foldMap (Sig.runStateMachine stateMachine) (ByteString.unpack bytes)
