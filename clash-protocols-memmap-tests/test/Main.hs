-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RankNTypes #-}
module Main (main) where

import Clash.Prelude

import Control.Monad (forM_)

import Protocols.MemoryMap (MemoryMap)
import Clash.Class.BitPackC (ByteOrder (..))

import qualified Protocols.MemoryMap.Test.Instances.UartMock as UartMock
import qualified Protocols.MemoryMap.Test.Instances.InterconnectTypeTests as ITT

withBO :: ByteOrder -> ByteOrder -> ((?regByteOrder :: ByteOrder, ?busByteOrder :: ByteOrder) => a) -> a
withBO reg bus val =
  let
    ?regByteOrder = reg
    ?busByteOrder = bus
  in val

memoryMapGeneration :: [MemoryMap]
memoryMapGeneration =
  [ UartMock.mm
  , withBO LittleEndian BigEndian ITT.mm
  ]

main :: IO ()
main = do
  forM_ memoryMapGeneration $ \mm -> do
    print mm
