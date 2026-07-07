-- SPDX-FileCopyrightText: 2026 QBayLogic B.V.
--
-- SPDX-License-Identifier: Apache-2.0

module Tests.Protocols.MemoryMap.RegisterOverlap where

import Clash.Explicit.Prelude

import Protocols.MemoryMap
import Protocols.MemoryMap.Check

import qualified Data.List as L
import qualified Data.Map.Strict as Map

import Test.Tasty
import Test.Tasty.HUnit (Assertion, testCase, (@?=))

-- to make it easier to define registers
named :: String -> a -> NamedLoc a
named nameString val = NamedLoc { name = Name nameString "", loc = locHere, value = val }

-- a "dummy" memorymap that contains a single device, tree is 'undefined'
mmWithRegs :: [NamedLoc Register] -> MemoryMap
mmWithRegs regs = MemoryMap
  { deviceDefs = Map.singleton "OverlappingDevice" dev
  , tree = undefined
  }
 where
  dev = DeviceDefinition
    { deviceName = Name "OverlappingDevice" ""
    , registers = regs
    , definitionLoc = locHere
    , tags = []
    }

-- | When there are no registers there should be no overlap!
case_noRegsNoErrors :: Assertion
case_noRegsNoErrors = do
  let errs = checkRegisters mm
  errs @?= []
 where
  mm = mmWithRegs []

-- | When there is a single overlap, there should be a single error!
case_singleOverlapErrorCaught :: Assertion
case_singleOverlapErrorCaught = do
  let errs = checkRegisters mm
  L.length errs @?= 1
  case errs of
    [RegisterOverlapsWithAnother
      { overlapsRegister
      , overlapsAddress
      , overlapsSize
      , nextRegister
      , nextAddress }] -> do
        overlapsRegister.name @?= "a"
        nextRegister.name @?= "b"

        overlapsAddress @?= 0
        nextAddress @?= 4
        overlapsSize @?= 8

    _ -> error "Expected a single RegisterOverlapsWithAnother error"
 where
  mm = mmWithRegs
    [ named "a" $ Register
        { access = ReadOnly
        , address = 0
        , fieldType = regType @(Vec 8 (BitVector 8))
        , reset = Nothing
        , tags = []
        }
    , named "b" $ Register
        { access = ReadOnly
        , address = 4 -- not right!
        , fieldType = regType @(Vec 8 (BitVector 8))
        , reset = Nothing
        , tags = []
        }
    ]

-- | When there are two overlapping registers, there should be two errors!
case_twoOverlapErrorsCaught :: Assertion
case_twoOverlapErrorsCaught = do
  let errs = checkRegisters mm
  L.length errs @?= 2
 where
  mm = mmWithRegs
    [ named "a" $ Register
        { access = ReadOnly
        , address = 0
        , fieldType = regType @(Vec 8 (BitVector 8))
        , reset = Nothing
        , tags = []
        }
    , named "b" $ Register
        { access = ReadOnly
        , address = 4 -- not right!
        , fieldType = regType @(Vec 8 (BitVector 8))
        , reset = Nothing
        , tags = []
        }
    , named "c" $ Register
        { access = ReadOnly
        , address = 8 -- also not right!
        , fieldType = regType @(Vec 8 (BitVector 8))
        , reset = Nothing
        , tags = []
        }
    ]

-- | When a register is not aligned properly, there should be an error!
case_unalginedRegisterCaught :: Assertion
case_unalginedRegisterCaught = do
  let errs = checkRegisters mm
  L.length errs @?= 1
  case errs of
    [RegisterIsUnaligned
      { register = reg
      , alignment
      , address }] -> do
        reg.name @?= "a"
        address @?= 0x1
        alignment @?= 8

    _ -> error "Expected a single RegisterIsUnaligned error"
 where
  mm = mmWithRegs
    [ named "a" $ Register
        { access = ReadOnly
        , address = 0x1
        , fieldType = regType @(Signed 64)
        , reset = Nothing
        , tags = []
        }
    ]

-- | When there are two registers, but they don't overlap each other,
--   there should be no error!
case_noOverlapNoError :: Assertion
case_noOverlapNoError = do
  let errs = checkRegisters mm
  errs @?= []
 where
  mm = mmWithRegs
    [ named "a" $ Register
        { access = ReadOnly
        , address = 0
        , fieldType = regType @(Vec 8 (BitVector 8))
        , reset = Nothing
        , tags = []
        }
    , named "b" $ Register
        { access = ReadOnly
        , address = 8
        , fieldType = regType @(Signed 64)
        , reset = Nothing
        , tags = []
        }
    ]

tests :: TestTree
tests =
  testGroup
    "RegisterOverlap"
    [ testCase "case_noRegsNoErrors" case_noRegsNoErrors
    , testCase "case_singleOverlapErrorCaught" case_singleOverlapErrorCaught
    , testCase "case_twoOverlapErrorsCaught" case_twoOverlapErrorsCaught
    , testCase "case_unalginedRegisterCaught" case_unalginedRegisterCaught
    , testCase "case_noOverlapNoError" case_noOverlapNoError
    ]
