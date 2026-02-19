-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# OPTIONS_GHC -fplugin Protocols.Plugin #-}
module Protocols.MemoryMap.Test.Instances.InterconnectTypeTests where

import Clash.Prelude hiding (read)

import GHC.Stack (HasCallStack)
import Protocols (Circuit, ToConstBwd)
import Protocols.MemoryMap (Mm, withName, getMMAny, MemoryMap, unMemmap)
import Protocols.Wishbone (Wishbone, WishboneMode(..))
import Protocols.MemoryMap.Registers.WishboneStandard (deviceWb, registerConfig, registerWbI_)
import Clash.Class.BitPackC (ByteOrder)
import Protocols.MemoryMap.Test.Interconnect (interconnect)

import Protocols.MemoryMap.Test.Hedgehog.WbTransaction
import Text.Printf (printf)
import Data.Functor.Identity (Identity(runIdentity))

mm ::
  (?regByteOrder :: ByteOrder, ?busByteOrder :: ByteOrder) =>
  MemoryMap
mm = getMMAny $ withClockResetEnable @System clockGen resetGen enableGen interconnectTypeTests

interconnectTypeTests ::
  forall dom.
  (HasCallStack, HiddenClockResetEnable dom, HasCallStack) =>
  (?regByteOrder :: ByteOrder, ?busByteOrder :: ByteOrder) =>
  Circuit (ToConstBwd Mm, Wishbone dom 'Standard 32 4) ()
interconnectTypeTests = circuit $ \(mm, master) -> do
  [prim] <- interconnect -< (mm, master)
  [primA, primB] <- withName "Primitives" interconnect -< prim
  withName "A" registerPrimitivesTest -< primA
  withName "B" registerPrimitivesTest -< primB

registerPrimitivesTest ::
  (HasCallStack, HiddenClockResetEnable dom, KnownNat addrWidth) =>
  (?regByteOrder :: ByteOrder, ?busByteOrder :: ByteOrder) =>
  Circuit (ToConstBwd Mm, Wishbone dom 'Standard addrWidth 4) ()
registerPrimitivesTest = circuit $ \(mm, wb) -> do
  [   bool
    , unit
    , bv8
    , bv34
    , u8
    , u24
    , s8
    , s31
    , i100
    , i38390
    , bv8_arr5
    , bv8_arr5_arr2
    , tup_bool_unit
    ] <- deviceWb "RegisterPrimitivesTest" -< (mm, wb)

  registerWbI_ (registerConfig "bool" "") False -< (bool, Fwd noWrite)
  registerWbI_ (registerConfig "unit" "") () -< (unit, Fwd noWrite)

  registerWbI_ (registerConfig "bv8" "") (0 :: BitVector 8) -< (bv8, Fwd noWrite)
  registerWbI_ (registerConfig "bv34" "") (0 :: BitVector 34) -< (bv34, Fwd noWrite)

  registerWbI_ (registerConfig "u8" "") (0 :: Unsigned 8) -< (u8, Fwd noWrite)
  registerWbI_ (registerConfig "u24" "") (0 :: Unsigned 24) -< (u24, Fwd noWrite)

  registerWbI_ (registerConfig "s8" "") (0 :: Signed 8) -< (s8, Fwd noWrite)
  registerWbI_ (registerConfig "s31" "") (0 :: Signed 31) -< (s31, Fwd noWrite)

  registerWbI_ (registerConfig "i100" "") (0 :: Index 100) -< (i100, Fwd noWrite)
  registerWbI_ (registerConfig "i38390" "") (0 :: Index 38390) -< (i38390, Fwd noWrite)

  registerWbI_ (registerConfig "bv8_arr5" "") (repeat 0 :: Vec 5 (BitVector 8)) -< (bv8_arr5, Fwd noWrite)
  registerWbI_ (registerConfig "bv8_arr5_arr2" "") (repeat (repeat 0) :: Vec 2 (Vec 5 (BitVector 8))) -< (bv8_arr5_arr2, Fwd noWrite)

  registerWbI_ (registerConfig "tup_bool_unit" "") (False, ()) -< (tup_bool_unit, Fwd noWrite)
  where
    noWrite = pure Nothing


boolTest ::
  (?regByteOrder :: ByteOrder, ?busByteOrder :: ByteOrder) =>
  Transaction TestDone
boolTest = do
  res0 <- read 0x00
  assert (res0 == False) "intial value should be False"
  write 0x00 True
  res1 <- read 0x00
  assert (res1 == True) "updated value should be True"
  testSucceeded

bv8Test ::
  (?regByteOrder :: ByteOrder, ?busByteOrder :: ByteOrder) =>
  Transaction TestDone
bv8Test = do
  res0 :: BitVector 8 <- read 0x08
  assert (res0 == 0) "intial value should be 0"
  write 0x08 (42 :: BitVector 8)
  res1 :: BitVector 8 <- read 0x08
  assert (res1 == 42) "updated value should be 42"
  testSucceeded

bv34Test ::
  (?regByteOrder :: ByteOrder, ?busByteOrder :: ByteOrder) =>
  Transaction TestDone
bv34Test = do
  res0 :: BitVector 34 <- read 0x0C
  assert (res0 == 0) "intial value should be 0"
  write 0x0C (0xABCD :: BitVector 34)
  res1 :: BitVector 34 <- read 0x0C
  assert (res1 == 0xABCD) $ printf "updated value should be 0xABCD but is %X" (toInteger res1)
  testSucceeded

runInterconnectTypeTests :: (?regByteOrder :: ByteOrder, ?busByteOrder :: ByteOrder) => [(String, TransactionResult)]
runInterconnectTypeTests = runIdentity $ runWbTest circuit1 testList
 where
  testList =
    [ ("Bool", boolTest)
    , ("bv8", bv8Test)
    , ("bv34", bv34Test)
    ]
  circuit0 = withClockResetEnable @System clockGen resetGen enableGen interconnectTypeTests
  circuit1 = unMemmap circuit0
