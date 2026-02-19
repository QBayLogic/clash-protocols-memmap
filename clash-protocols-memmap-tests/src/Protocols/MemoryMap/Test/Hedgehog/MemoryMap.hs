-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
module Protocols.MemoryMap.Test.Hedgehog.MemoryMap where

import Clash.Explicit.Prelude


import qualified Data.List as L
import qualified Data.Map.Strict as Map

import Protocols.MemoryMap (
  Address,
  MemoryMap (..),
  Register (..), DeviceDefinition (..), DeviceDefinitions, Path, MemoryMapTreeAnn (AnnInterconnect, AnnDeviceInstance), NamedLoc,
 )
import Protocols.MemoryMap.Check.AbsAddress (MemoryMapTreeAbsNorm, AbsNormData (..))


data TraversedRegister = TraversedRegister
  { regDesc :: NamedLoc Register
  , instancePath :: Path
  , instanceAddr :: Address
  , deviceDef :: DeviceDefinition
  }

traverseRegisters :: MemoryMap -> MemoryMapTreeAbsNorm -> [TraversedRegister]
traverseRegisters mm = traverseTree mm.deviceDefs
 where
  traverseTree :: DeviceDefinitions -> MemoryMapTreeAbsNorm -> [TraversedRegister]
  traverseTree defs (AnnInterconnect _ _ (fmap snd -> comps)) = L.concatMap (traverseTree defs) comps
  traverseTree defs (AnnDeviceInstance absData _ name) = findRegs absData (defs Map.! name)

  findRegs :: AbsNormData -> DeviceDefinition -> [TraversedRegister]
  findRegs AbsNormData{path, absoluteAddr} devDef = flip fmap devDef.registers $ \reg ->
    TraversedRegister { regDesc = reg, instancePath = path, instanceAddr = absoluteAddr, deviceDef = devDef }
