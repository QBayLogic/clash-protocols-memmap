-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE AllowAmbiguousTypes #-}

module Tests.Protocols.MemoryMap.MergeDeviceDefs where

import Clash.Prelude

import Clash.Class.BitPackC (BitPackC)
import Control.Exception (ErrorCall, evaluate, try)
import Protocols.MemoryMap
import Protocols.MemoryMap.TypeDescription (WithTypeDescription)
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Map.Strict as Map

{- | A device named @Reg@ with one register of type @a@, as a parametric
component would produce it for each of its instantiations.
-}
regDevice ::
  forall a. (HasCallStack, WithTypeDescription a, BitPackC a) => DeviceDefinitions
regDevice =
  deviceSingleton
    DeviceDefinition
      { deviceName = Name "Reg" ""
      , registers =
          [ NamedLoc
              { name = Name "value" ""
              , loc = locHere
              , value =
                  Register
                    { access = ReadWrite
                    , address = 0
                    , fieldType = regType @a
                    , reset = Nothing
                    , tags = []
                    }
              }
          ]
      , definitionLoc = locHere
      , tags = []
      }

-- | Two instances of the same device share one definition.
case_identicalDefinitionsMerge :: Assertion
case_identicalDefinitionsMerge =
  Map.keys (mergeDeviceDefs [regDevice @(Unsigned 8), regDevice @(Unsigned 8)])
    @?= ["Reg"]

{- | Two different devices with the same name must not be merged silently: one
of the two register layouts would be lost.
-}
case_conflictingDefinitionsAreRejected :: Assertion
case_conflictingDefinitionsAreRejected = do
  result <-
    try @ErrorCall
      $ evaluate
      $ mergeDeviceDefs [regDevice @(Unsigned 8), regDevice @(Unsigned 16)]
  case result of
    Left _ -> pure ()
    Right merged ->
      assertFailure
        $ "expected an error, but the merge kept only: "
        <> show [(r.name.name, r.value.fieldType) | r <- (merged Map.! "Reg").registers]

tests :: TestTree
tests =
  testGroup
    "MergeDeviceDefs"
    [ testCase "identical definitions merge" case_identicalDefinitionsMerge
    , testCase "conflicting definitions are rejected" case_conflictingDefinitionsAreRejected
    ]
