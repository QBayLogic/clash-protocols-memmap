-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE AllowAmbiguousTypes #-}


module Protocols.MemoryMap.Test.Hedgehog.WbTransaction where

import Clash.Explicit.Prelude hiding (singleton)

import Clash.Class.BitPackC

import Control.Monad.Operational

import Data.Constraint (Dict (..))

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
import Unsafe.Coerce (unsafeCoerce)
import Clash.Sized.Vector.ToTuple (VecToTuple(vecToTuple))
import Data.Tuple (Solo(MkSolo))

data OrderedWord (byteOrder :: ByteOrder) where
  Word :: BitVector 32 -> OrderedWord byteOrder

data TestDone = TestDone

data TestInstruction a where
  ReadFrom :: Address -> BitVector 4 -> TestInstruction (WishboneS2MResult (BitVector 32))
  WriteTo :: Address -> BitVector 4 -> BitVector 32 -> TestInstruction (WishboneS2MResult ())
  Idle :: TestInstruction ()
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

idle :: TransactionT m ()
idle = singleton Idle

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

{-| Words spit out are in ?regByteOrder -}
readNWordsFromAligned ::
  forall n m. (Monad m, ?regByteOrder :: ByteOrder, ?busByteOrder :: ByteOrder) =>
  SNat n ->
  Address ->
  TransactionT m (Vec n (BitVector 32))
readNWordsFromAligned SNat addr = do
  let wordIndices = iterateI @n (+1) 0
  let wordAlignedAddr = addr `div` 4
  forM wordIndices $ \i -> do
    let currentWordAddr = wordAlignedAddr + i
    word0 <- readRaw currentWordAddr 0b1111
    pure $ shuffle word0

readAligned ::
  forall a m.
  (BitPack a, BitPackC a, Monad m, ?regByteOrder :: ByteOrder, ?busByteOrder :: ByteOrder) =>
  Address ->
  TransactionT m a
readAligned addr = do
  -- output is in regByteOrder
  words0 <- readNWordsFromAligned SNat addr

  let x = maybeUnpackWordC ?regByteOrder words0
  case x of
    Nothing -> testFailed $ printf "Error when trying to unpack value from memory at %X" addr
    Just val -> do
      pure val

-- | Takes word in regByteOrder and outputs word in regByteOrder
extractDataFromWord :: (?regByteOrder :: ByteOrder) => BitVector 32 -> Integer -> Integer -> BitVector 32
extractDataFromWord word size offset
  | BigEndian <- ?regByteOrder =
    case (size, offset, unpack word :: Vec 4 (BitVector 8)) of
      (1, 0, _ :> _ :> _ :> a :> Nil) -> pack $ a :> 0 :> 0 :> 0 :> Nil
      (1, 1, _ :> _ :> a :> _ :> Nil) -> pack $ a :> 0 :> 0 :> 0 :> Nil
      (1, 2, _ :> a :> _ :> _ :> Nil) -> pack $ a :> 0 :> 0 :> 0 :> Nil
      (1, 3, a :> _ :> _ :> _ :> Nil) -> pack $ a :> 0 :> 0 :> 0 :> Nil
      (2, 0, _ :> _ :> a :> b :> Nil) -> pack $ b :> a :> 0 :> 0 :> Nil
      (2, 1, _ :> a :> b :> _ :> Nil) -> pack $ b :> a :> 0 :> 0 :> Nil
      (2, 2, a :> b :> _ :> _ :> Nil) -> pack $ b :> a :> 0 :> 0 :> Nil
      (3, 0, _ :> a :> b :> c :> Nil) -> pack $ c :> b :> a :> 0 :> Nil
      (3, 1, a :> b :> c :> _ :> Nil) -> pack $ c :> b :> a :> 0 :> Nil
      _ -> word
  | otherwise = -- LittleEndian
    case (size, offset, unpack word :: Vec 4 (BitVector 8)) of
      (1, 0, _ :> _ :> _ :> a :> Nil) -> pack $ a :> 0 :> 0 :> 0 :> Nil
      (1, 1, _ :> _ :> a :> _ :> Nil) -> pack $ a :> 0 :> 0 :> 0 :> Nil
      (1, 2, _ :> a :> _ :> _ :> Nil) -> pack $ a :> 0 :> 0 :> 0 :> Nil
      (1, 3, a :> _ :> _ :> _ :> Nil) -> pack $ a :> 0 :> 0 :> 0 :> Nil
      (2, 0, _ :> _ :> b :> a :> Nil) -> pack $ a :> b :> 0 :> 0 :> Nil
      (2, 1, _ :> b :> a :> _ :> Nil) -> pack $ a :> b :> 0 :> 0 :> Nil
      (2, 2, b :> a :> _ :> _ :> Nil) -> pack $ a :> b :> 0 :> 0 :> Nil
      (3, 0, _ :> c :> b :> a :> Nil) -> pack $ a :> b :> c :> 0 :> Nil
      (3, 1, c :> b :> a :> _ :> Nil) -> pack $ a :> b :> c :> 0 :> Nil
      (_, _, unpackedWord) -> pack $ reverse unpackedWord

{-| Incoming word is regByteOrder, outputs in regByteOrder -}
packageDataIntoWord ::
  (?regByteOrder :: ByteOrder) =>
  BitVector 32 ->
  Integer ->
  Integer ->
  (BitVector 32, BitVector 4)
packageDataIntoWord word size offset
  | BigEndian <- ?regByteOrder =
    case (size, offset, unpack word :: Vec 4 (BitVector 8)) of
      (1, 0, _ :> _ :> _ :> a :> Nil) -> (pack $ 0 :> 0 :> 0 :> a :> Nil, 0b0001)
      (1, 1, _ :> _ :> _ :> a :> Nil) -> (pack $ 0 :> 0 :> a :> 0 :> Nil, 0b0010)
      (1, 2, _ :> _ :> _ :> a :> Nil) -> (pack $ 0 :> a :> 0 :> 0 :> Nil, 0b0100)
      (1, 3, _ :> _ :> _ :> a :> Nil) -> (pack $ a :> 0 :> 0 :> 0 :> Nil, 0b1000)
      (2, 0, _ :> _ :> a :> b :> Nil) -> (pack $ 0 :> 0 :> a :> b :> Nil, 0b0011)
      (2, 1, _ :> _ :> a :> b :> Nil) -> (pack $ 0 :> a :> b :> 0 :> Nil, 0b0110)
      (2, 2, _ :> _ :> a :> b :> Nil) -> (pack $ a :> b :> 0 :> 0 :> Nil, 0b1100)
      (3, 0, _ :> a :> b :> c :> Nil) -> (pack $ 0 :> a :> b :> c :> Nil, 0b0111)
      (3, 1, _ :> a :> b :> c :> Nil) -> (pack $ a :> b :> c :> 0 :> Nil, 0b1110)
      _ -> (word, 0b1111)
  | otherwise = -- LittleEndian
    case (size, offset, unpack word :: Vec 4 (BitVector 8)) of
      (1, 0, _ :> _ :> _ :> a :> Nil) -> (pack $ 0 :> 0 :> 0 :> a :> Nil, 0b0001)
      (1, 1, _ :> _ :> _ :> a :> Nil) -> (pack $ 0 :> 0 :> a :> 0 :> Nil, 0b0010)
      (1, 2, _ :> _ :> _ :> a :> Nil) -> (pack $ 0 :> a :> 0 :> 0 :> Nil, 0b0100)
      (1, 3, _ :> _ :> _ :> a :> Nil) -> (pack $ a :> 0 :> 0 :> 0 :> Nil, 0b1000)
      (2, 0, _ :> _ :> a :> b :> Nil) -> (pack $ 0 :> 0 :> a :> b :> Nil, 0b0011)
      (2, 1, _ :> _ :> a :> b :> Nil) -> (pack $ 0 :> a :> b :> 0 :> Nil, 0b0110)
      (2, 2, _ :> _ :> a :> b :> Nil) -> (pack $ a :> b :> 0 :> 0 :> Nil, 0b1100)
      (3, 0, _ :> a :> b :> c :> Nil) -> (pack $ 0 :> a :> b :> c :> Nil, 0b0111)
      (3, 1, _ :> a :> b :> c :> Nil) -> (pack $ a :> b :> c :> 0 :> Nil, 0b1110)
      (_, _, unpackedWord) -> (pack unpackedWord, 0b1111)

readLessThanWord ::
  forall a m.
  (BitPackC a, Monad m, ?regByteOrder :: ByteOrder, ?busByteOrder :: ByteOrder) =>
  (ByteSizeC a <= 4) =>
  Address ->
  TransactionT m a
readLessThanWord addr = do
  let size = natToInteger @(ByteSizeC a)
  let subWordAddr = addr `mod` 4
  let wordAlignedAddr = addr `div` 4 -- subWordAddr
  word0 <- readRaw wordAlignedAddr 0b1111
  let word1 = extractDataFromWord word0 size subWordAddr
  case cancelMulDiv @(ByteSizeC a) @4 of
    Dict -> case maybeUnpackWordC ?regByteOrder (word1 :> Nil) of
     Nothing -> testFailed $ printf "Error when trying to unpack value from memory at %X" addr
     Just val -> do
       pure val

read ::
  forall a m.
  (BitPack a, BitPackC a, Monad m, ?regByteOrder :: ByteOrder, ?busByteOrder :: ByteOrder) =>
  Address ->
  TransactionT m a
read addr = do
  case compareSNat (SNat @(ByteSizeC a)) (SNat @4) of
    SNatLE -> readLessThanWord addr
    _ -> do
      let align = natToInteger @(AlignmentBoundaryC a)
      if (addr `mod` align) /= 0 then
        testFailed $ printf "Unaligned read at addr %08X (expected alignment %i)" (toInteger addr) align
      else
        readAligned addr

writeAligned ::
  forall a m.
  (BitPackC a, Monad m, ?regByteOrder :: ByteOrder, ?busByteOrder :: ByteOrder) =>
  Address ->
  a ->
  TransactionT m ()
writeAligned addr val = do
  let wordAlignedAddr = addr `div` 4
  let bytes = natToInteger @(ByteSizeC a)
  let x = packWordC @4 ?regByteOrder val
  forM_ (iterateI (+ 1) 0 `zip` x) $ \(i, word0) -> do
    let word1 = shuffle word0
    let bytesSoFar = i * 4
    let bytesLeft = bytes - bytesSoFar
    let bytesInWord = bytesLeft `min` 4
    let offset = 0
    let (word2, sel) = packageDataIntoWord word1 bytesInWord offset
    writeRaw (wordAlignedAddr + i) sel word2

writeLessThanWord ::
  forall a m.
  (BitPackC a, Monad m, ?regByteOrder :: ByteOrder, ?busByteOrder :: ByteOrder) =>
  (ByteSizeC a <= 4) =>
  Address ->
  a ->
  TransactionT m ()
writeLessThanWord addr val = do
  let size = natToInteger @(ByteSizeC a)
  let subWordAddr = addr `mod` 4
  let wordAlignedAddr = addr `div` 4
  case cancelMulDiv @(ByteSizeC a) @4 of
    Dict -> do
      let (vecToTuple -> (MkSolo word0)) = packWordC @4 ?regByteOrder val
      let word1 = shuffle word0
      let (word2, sel) = packageDataIntoWord word1 size subWordAddr
      writeRaw wordAlignedAddr sel word2

write ::
  forall a m.
  (BitPackC a, Monad m, ?regByteOrder :: ByteOrder, ?busByteOrder :: ByteOrder) =>
  Address ->
  a ->
  TransactionT m ()
write addr val = do
  case compareSNat (SNat @(ByteSizeC a)) (SNat @4) of
    SNatLE -> writeLessThanWord addr val
    _ -> do
      let align = natToInteger @(AlignmentBoundaryC a)
      if (addr `mod` align) /= 0 then
        testFailed $ printf "Unaligned read at addr %08X (expected alignment %i)" (toInteger addr) align
      else
        writeAligned addr val

shuffle :: (?regByteOrder :: ByteOrder, ?busByteOrder :: ByteOrder) => BitVector 32 -> BitVector 32
shuffle bv
  | ?regByteOrder /= ?busByteOrder = pack $ reverse (unpack bv :: Vec 4 (BitVector 8))
  | otherwise = bv


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
  runTransaction automaton0@(Automaton step0) testName instructions = case instructions of
    Return TestDone -> pure (automaton0, Success)
    TestSucceeded :>>= _ -> pure (automaton0, Success)
    TestFailed msg :>>= _ -> pure (automaton0, Failure msg)
    Idle :>>= next -> do
      let (_s2m, automaton1) = step0 emptyWishboneM2S
      let nextInstr = next ()
      nextView <- viewT nextInstr
      runTransaction automaton1 testName nextView
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


cancelMulDiv :: forall a b. (1 <= b, a <= b) => Dict (DivRU a b ~ 1)
cancelMulDiv = unsafeCoerce (Dict :: Dict (0 ~ 0))
