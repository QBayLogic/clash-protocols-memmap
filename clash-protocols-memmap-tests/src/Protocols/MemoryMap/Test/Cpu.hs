-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
module Protocols.MemoryMap.Test.Cpu where

import Clash.Prelude

import Protocols
import Protocols.MemoryMap
import Protocols.Wishbone

import Clash.Class.BitPackC

import VexRiscv (CpuIn (..), CpuOut (..), DumpVcd, JtagIn (..), JtagOut (..), Jtag)
import VexRiscv.Reset (MinCyclesReset)

import qualified VexRiscv_TestCpu


testCpu ::
  forall dom. (KnownDomain dom) =>
  (?busByteOrder :: ByteOrder, ?regByteOrder :: ByteOrder) =>
  DumpVcd ->
  Clock dom ->
  MinCyclesReset dom 2 ->
  Circuit
    (ToConstBwd Mm, Jtag dom)
    ( Wishbone dom 'Standard 30 4
    , (ToConstBwd Mm, Wishbone dom 'Standard 30 4)
    )
testCpu dumpVcd clk reset = case (?busByteOrder, ?regByteOrder) of
  (BigEndian, LittleEndian) -> Circuit go
  (_busByteOrder, _regByteOrder) ->
      clashCompileError $ "Unsupported bus- and register byte order, only bus = BigEndian, reg = LittleEndian is supported"
 where
  go (((), jtagIn), (iBusIn, (mm, dBusIn))) = ((mm, jtagOut), (iBusWbM2S <$> cpuOut, ((), dBusWbM2S <$> cpuOut)))
   where
     (cpuOut, jtagOut) = testCpu' dumpVcd clk reset cpuIn jtagIn
     makeCpuIn iBus dBus = CpuIn { timerInterrupt = low, softwareInterrupt = low, externalInterrupt = low, iBusWbS2M = iBus, dBusWbS2M = dBus }
     cpuIn = makeCpuIn <$> iBusIn <*> dBusIn

testCpu' ::
  forall dom.
  (KnownDomain dom) =>
  DumpVcd ->
  Clock dom ->
  MinCyclesReset dom 2 ->
  Signal dom CpuIn ->
  Signal dom JtagIn ->
  ( Signal dom CpuOut
  , Signal dom JtagOut
  )
testCpu' = VexRiscv_TestCpu.vexRiscv
