-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# OPTIONS_GHC -fplugin Protocols.Plugin #-}
module Protocols.MemoryMap.Test.Instances.InterconnectTypeTests where

import Clash.Prelude

import GHC.Stack (HasCallStack)
import Protocols (Circuit, ToConstBwd)
import Protocols.MemoryMap (Mm, withName, getMMAny, MemoryMap)
import Protocols.Wishbone (Wishbone, WishboneMode(..))
import Protocols.MemoryMap.Registers.WishboneStandard (deviceWb, registerConfig, registerWbI_)
import Clash.Class.BitPackC (ByteOrder)
import Protocols.MemoryMap.Test.Interconnect (interconnect)

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
