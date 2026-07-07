-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE RecordWildCards #-}

-- | Validity checks performed on memory maps
module Protocols.MemoryMap.Check (
  module Protocols.MemoryMap.Check.AbsAddress,
  module Protocols.MemoryMap.Check.Normalized,
  module Protocols.MemoryMap.Check.RegisterOverlap,
  getAddrErrorMessage,
  getRegErrorMessage,
) where

import Prelude

import GHC.Stack (SrcLoc (..))
import Protocols.MemoryMap
import Protocols.MemoryMap.Check.AbsAddress
import Protocols.MemoryMap.Check.Normalized
import Protocols.MemoryMap.Check.RegisterOverlap
import Text.Printf (printf)

getAddrErrorMessage :: AddressError -> String
getAddrErrorMessage err =
  case err of
    SizeExceedsError
      { startAddr
      , availableSize
      , requestedSize
      , path
      , location
      } ->
        printf
          "Component %s at address %08X with size %08X exceeds the available size %08X (%s)"
          (show path)
          startAddr
          requestedSize
          availableSize
          (shortLocation location)
    AddressDifferentThanExpected
      { expected
      , actual
      , path
      , location
      } ->
        printf
          "Component %s has been given an absolute address %08X which is different from the computed one %08X (%s)"
          (show path)
          expected
          actual
          (shortLocation location)

getRegErrorMessage :: RegisterCheckError -> String
getRegErrorMessage err =
  case err of
    RegisterIsUnaligned{..} ->
      printf
        "In device %s the register %s is not aligned properly. Alignment requirement by type = %d, Address %08X (%s)"
        device
        register.name
        alignment
        address
        (shortLocation loc)

    RegisterOverlapsWithAnother{..} ->
      printf
        "In device %s the register %s (address %08X, size = %d) (%s) overlaps with register %s (address %08X) (%s)"
        device
        overlapsRegister.name
        overlapsAddress
        overlapsSize
        (shortLocation overlapsLoc)
        nextRegister.name
        nextAddress
        (shortLocation nextLoc)

shortLocation :: SrcLoc -> String
shortLocation s@SrcLoc{} = s.srcLocFile <> ":" <> show s.srcLocStartLine <> ":" <> show s.srcLocStartCol
