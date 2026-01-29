-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE TypeApplications #-}
module Main (main) where

import Clash.Prelude

import Protocols.MemoryMap (getMMAny, MemoryMap)

import Protocols.MemoryMap.Test.Instances.UartMock as UartMock
import Control.Monad (forM_)

memoryMapGeneration :: [MemoryMap]
memoryMapGeneration =
  [
      getMMAny (withClockResetEnable clockGen resetGen enableGen $ UartMock.someCircuit @System)
  ]

main :: IO ()
main = do
  forM_ memoryMapGeneration $ \mm -> do
    print mm
