-- SPDX-FileCopyrightText: 2026 QBayLogic B.V.
--
-- SPDX-License-Identifier: Apache-2.0

module Protocols.MemoryMap.Check.RegisterOverlap (
  RegisterCheckError (..),
  checkRegisters,
) where

import Prelude

import qualified Data.Map.Strict as Map
import Data.List (sortOn)

import GHC.Stack (SrcLoc)

import Protocols.MemoryMap

data RegisterCheckError
  = RegisterIsUnaligned
    { device :: DeviceName
    , register :: Name
    , loc :: SrcLoc
    , alignment :: Integer
    , address :: Address
    }
  | RegisterOverlapsWithAnother
    { device :: DeviceName
    , overlapsRegister :: Name
    , overlapsLoc :: SrcLoc
    , overlapsAddress :: Address
    , overlapsSize :: Integer
    , nextRegister :: Name
    , nextLoc :: SrcLoc
    , nextAddress :: Address
    }
  deriving (Eq, Show)

checkRegisters :: MemoryMap -> [RegisterCheckError]
checkRegisters m = concat results
  where
    deviceDefs = m.deviceDefs
    devices = Map.toList deviceDefs

    results = map (uncurry checkDevice) devices

checkDevice :: DeviceName -> DeviceDefinition -> [RegisterCheckError]
checkDevice name def = go regs1
  where
    regs0 = def.registers
    regs1 = sortOn (\reg -> reg.value.address) regs0

    go [] = []
    go [reg] = checkReg reg Nothing
    go (a:b:rest) = checkReg a (Just b) <> go (b:rest)

    checkReg :: NamedLoc Register -> Maybe (NamedLoc Register) -> [RegisterCheckError]
    checkReg current next = overlapError <> alignmentError
      where
        currentAddr = current.value.address
        currentSize = regByteSizeC current.value.fieldType
        currentAlignment = regByteAlignment current.value.fieldType

        overlapError
          | Just nextReg <- next,
            currentAddr + currentSize > nextReg.value.address =
              [RegisterOverlapsWithAnother
                { device = name
                , overlapsRegister = current.name
                , overlapsLoc = current.loc
                , overlapsAddress = currentAddr
                , overlapsSize = currentSize
                , nextRegister = nextReg.name
                , nextLoc = nextReg.loc
                , nextAddress = nextReg.value.address
                }
              ]
          | otherwise = []

        alignmentError
          | currentAddr `mod` currentAlignment == 0 = []
          | otherwise =
            [RegisterIsUnaligned
              { device = name
              , register = current.name
              , loc = current.loc
              , alignment = currentAlignment
              , address = currentAddr
              }
            ]
