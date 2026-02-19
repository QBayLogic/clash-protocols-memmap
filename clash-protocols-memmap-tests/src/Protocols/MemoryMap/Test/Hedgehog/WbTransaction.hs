-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE RecordWildCards #-}


module Protocols.MemoryMap.Test.Hedgehog.WbTransaction where

import Clash.Explicit.Prelude hiding (singleton)

import Clash.Class.BitPackC

import Control.Monad.Operational


import Protocols (Circuit (..), toSignals)
import Protocols.MemoryMap (
  Address,
 )
import Protocols.Wishbone

import Control.Arrow.Transformer.Automaton (Automaton(Automaton))
import Text.Printf (printf)
import Clash.Class.BitPackC.Padding (packWordC, maybeUnpackWordC)
import Control.Monad (forM_, forM)
import Data.Functor.Identity (Identity)


data TestDone = TestDone

data TestInstruction a where
  ReadFrom :: Address -> BitVector 4 -> TestInstruction (WishboneS2MResult (BitVector 32))
  WriteTo :: Address -> BitVector 4 -> BitVector 32 -> TestInstruction (WishboneS2MResult ())
  TestSucceeded :: TestInstruction TestDone
  TestFailed :: String -> TestInstruction a

type TransactionT m a = ProgramT TestInstruction m a
type Transaction a = TransactionT Identity a

readRaw' :: Address -> BitVector 4 -> TransactionT m (WishboneS2MResult (BitVector 32))
readRaw' addr sel = singleton $ ReadFrom addr sel

writeRaw' :: Address -> BitVector 4 -> BitVector 32 -> TransactionT m (WishboneS2MResult ())
writeRaw' addr sel val = singleton $ WriteTo addr sel val

testSucceeded :: TransactionT m TestDone
testSucceeded = singleton TestSucceeded

testFailed :: String -> TransactionT m a
testFailed msg = singleton $ TestFailed msg

assert :: (Monad m) => Bool -> String -> TransactionT m ()
assert True _ = pure ()
assert False msg = testFailed $ printf "Assertion failed: %s" msg

readRaw :: (Monad m) => Address -> BitVector 4 -> TransactionT m (BitVector 32)
readRaw addr sel = do
  res <- readRaw' addr sel
  case res of
    Acknowlege val -> pure val
    Error -> testFailed $ printf "Bus signalled ERR on read (%X, %X)" (toInteger addr) (toInteger sel)
    Retry -> testFailed $ printf "Bus signalled RETRY on read (%X, %X)" (toInteger addr) (toInteger sel)

writeRaw :: (Monad m) => Address -> BitVector 4 -> BitVector 32 -> TransactionT m ()
writeRaw addr sel val = do
  res <- writeRaw' addr sel val
  case res of
    Acknowlege () -> pure ()
    Error -> testFailed $ printf "Bus signalled ERR on write (%X, %X) %X" (toInteger addr) (toInteger sel) (toInteger val)
    Retry -> testFailed $ printf "Bus signalled RETRY on write (%X, %X) %X" (toInteger addr) (toInteger sel) (toInteger val)

readNWordsFromUnaligned :: forall n m. (Monad m) => SNat n -> Address -> TransactionT m (Vec n (BitVector 32))
readNWordsFromUnaligned SNat addr = do
  forM (iterateI @n (+1) 0) $ \i -> do
    readRaw (addr + (i * 4)) 0b1111

readUnaligned ::
  (BitPackC a, Monad m, ?regByteOrder :: ByteOrder, ?busByteOrder :: ByteOrder) =>
  Address ->
  TransactionT m a
readUnaligned addr = do
  words0 <- readNWordsFromUnaligned SNat addr
  let words1 =
        if ?regByteOrder /= ?busByteOrder then
          shuffle <$> words0
        else
          words0

  let x = maybeUnpackWordC ?regByteOrder words1
  case x of
    Nothing -> testFailed $ printf "Error when trying to unpack value from memory at %X" addr
    Just val -> pure val
 where
  shuffle :: BitVector 32 -> BitVector 32
  shuffle bv = pack $ reverse (unpack bv :: Vec 4 (BitVector 8))

read ::
  forall a m.
  (BitPackC a, Monad m, ?regByteOrder :: ByteOrder, ?busByteOrder :: ByteOrder) =>
  Address ->
  TransactionT m a
read addr = do
  let align = natToInteger @(AlignmentBoundaryC a)
  if (addr `mod` align) /= 0 then
    testFailed $ printf "Unaligned read at addr (expected alignment %i)" align
  else
    readUnaligned addr

writeUnaligned ::
  (BitPackC a, Monad m, ?regByteOrder :: ByteOrder, ?busByteOrder :: ByteOrder) =>
  Address ->
  a ->
  TransactionT m ()
writeUnaligned addr val = do
  let x = packWordC @4 ?regByteOrder val
  forM_ (iterateI (+ 1) 0 `zip` x ) $ \(i, word0) -> do
    let word1 = if ?regByteOrder /= ?busByteOrder then shuffle word0 else word0
    writeRaw (addr + (i * 4)) 0b1111 word1
 where
  shuffle :: BitVector 32 -> BitVector 32
  shuffle bv = pack $ reverse (unpack bv :: Vec 4 (BitVector 8))

write ::
  forall a m.
  (BitPackC a, Monad m, ?regByteOrder :: ByteOrder, ?busByteOrder :: ByteOrder) =>
  Address ->
  a ->
  TransactionT m ()
write addr val = do
  let align = natToInteger @(AlignmentBoundaryC a)
  if (addr `mod` align) /= 0 then
    testFailed $ printf "Unaligned write at addr (expected alignment %i)" align
  else
    writeUnaligned addr val


data WishboneS2MResult a
  = Acknowlege a
  | Error
  | Retry

data TransactionResult
  = Success
  | Failure String
  deriving (Show)

type TestAutomaton addrWidth n = Automaton (->) (WishboneM2S addrWidth n) (WishboneS2M n)

runWbTest ::
  forall addrWidth dom m.
  (KnownNat addrWidth, 1 <= addrWidth, KnownDomain dom, Monad m) =>
  Circuit (Wishbone dom 'Standard addrWidth 4) () ->
  [(String, TransactionT m TestDone)] ->
  m [(String, TransactionResult)]
runWbTest circuit transactions = runAutomaton (signalAutomaton fn) transactions
 where
  fn = toInputOutput $ toSignals circuit

  toInputOutput :: ((a, ()) -> (b, ())) -> (a -> b)
  toInputOutput f = \input -> fst $ f (input, ())

  runAutomaton :: TestAutomaton addrWidth 4 -> [(String, TransactionT m TestDone)] -> m [(String, TransactionResult)]
  runAutomaton _ [] = pure []
  runAutomaton automaton0 ((name, transaction):rest) = do
    instrView <- viewT transaction
    (automaton1, res) <- runTransaction automaton0 name instrView
    restResults <- runAutomaton automaton1 rest
    pure $ (name, res) : restResults

  runTransaction ::
    TestAutomaton addrWidth 4 ->
    String ->
    ProgramViewT TestInstruction m TestDone ->
    m (TestAutomaton addrWidth 4, TransactionResult)
  runTransaction automaton0 testName instructions = case instructions of
    Return TestDone -> pure (automaton0, Success)
    TestSucceeded :>>= _ -> pure (automaton0, Success)
    TestFailed msg :>>= _ -> pure (automaton0, Failure msg)
    ReadFrom addr sel :>>= next -> do
      (automaton1, res) <- readData addr sel automaton0
      let nextInstr = next res
      nextView <- viewT nextInstr
      runTransaction automaton1 testName nextView

    WriteTo addr sel val :>>= next -> do
      (automaton1, res) <- writeData addr sel val automaton0
      let nextInstr = next res
      nextView <- viewT nextInstr
      runTransaction automaton1 testName nextView

  readData :: Address -> BitVector 4 -> TestAutomaton addrWidth 4 -> m (TestAutomaton addrWidth 4, WishboneS2MResult (BitVector 32))
  readData addr sel (Automaton step0) = waitForResult (step0 m2s)
   where
    m2s = (emptyWishboneM2S @addrWidth) { addr = fromInteger addr, busSelect = sel, busCycle = True, strobe = True, writeEnable = False }

    waitForResult (s2m, automaton@(Automaton step))
     | s2m.acknowledge = pure (automaton, Acknowlege s2m.readData)
     | s2m.err = pure (automaton, Error)
     | s2m.retry = pure (automaton, Retry)
     | otherwise = waitForResult (step m2s)

  writeData :: Address -> BitVector 4 -> BitVector 32 -> TestAutomaton addrWidth 4 -> m (TestAutomaton addrWidth 4, WishboneS2MResult ())
  writeData addr sel val (Automaton step0) = waitForResult (step0 m2s)
   where
    m2s = (emptyWishboneM2S @addrWidth @4) { addr = fromInteger addr, busSelect = sel, busCycle = True, strobe = True, writeEnable = True, writeData = val }

    waitForResult (s2m, automaton@(Automaton step))
     | s2m.acknowledge = pure (automaton, Acknowlege ())
     | s2m.err = pure (automaton, Error)
     | s2m.retry = pure (automaton, Retry)
     | otherwise = waitForResult (step m2s)
