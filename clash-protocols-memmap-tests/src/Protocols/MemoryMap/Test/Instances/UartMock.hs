-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# OPTIONS_GHC -fplugin Protocols.Plugin #-}
module Protocols.MemoryMap.Test.Instances.UartMock where

import Clash.Prelude

import GHC.Stack (HasCallStack)
import Protocols (Circuit, ToConstBwd, toSignals)
import Protocols.MemoryMap (Mm, withName, withPrefix)
import Protocols.Wishbone (Wishbone, WishboneMode(..), WishboneS2M, WishboneM2S)
import Protocols.MemoryMap.Registers.WishboneStandard (deviceWb, registerConfig, registerWbI_)
import Clash.Class.BitPackC (ByteOrder (BigEndian, LittleEndian))
import Protocols.MemoryMap.Test.Interconnect (interconnect)

topEntity ::
  Clock System ->
  Signal System (WishboneM2S 32 4) ->
  Signal System (WishboneS2M 4)
topEntity clk input = output
 where
  fn = toSignals (withClockResetEnable clk resetGen enableGen someCircuit)
  ((_mm, output), ()) = fn (((), input), ())


someCircuit ::
  forall dom.
  (HasCallStack, HiddenClockResetEnable dom, HasCallStack) =>
  Circuit (ToConstBwd Mm, Wishbone dom 'Standard 32 4) ()
someCircuit = circuit $ \(mm, master) -> do
  [a, b] <- interconnect -< (mm, master)
  withPrefix 0b01 (withName "A" magicUart) -< a
  withPrefix 0b00 (withName "B" magicUart) -< b

magicUart ::
  (HasCallStack, HiddenClockResetEnable dom, KnownNat addrWidth) =>
  Circuit (ToConstBwd Mm, Wishbone dom 'Standard addrWidth 4) ()
magicUart =
  let
    ?regByteOrder = LittleEndian
    ?busByteOrder = BigEndian
  in
  circuit $ \(mm, wb) -> do
    [dataWb] <- deviceWb "Uart" -< (mm, wb)
    registerWbI_ (registerConfig "data" "") (0 :: BitVector 8) -< (dataWb, Fwd (pure Nothing))
