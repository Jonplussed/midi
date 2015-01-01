{-# LANGUAGE OverloadedStrings #-}

module Midi where

import BitTools
import Data.Binary.Put
import Data.Bits ((.&.), setBit, shiftR)
import Data.ByteString.Char8
import Data.Word
import qualified Data.ByteString.Lazy as BL

-- These are somewhat like UTF-8 multibyte characters, if the high bit
-- of any byte is set, then it means another byte follows. Thus, the
-- highest single-byte value is: 127 == 0x7F == 0111 1111
--
-- Two-bytes go like this:
-- 128    == 1000 0001  0111 1111 (0x81 0x7F)
-- 129    == 1000 0010  0000 0000 (0x82 0x00)
-- ...
-- 16,382 == 1111 1111  0111 1110 (0xFF 0x7E)
-- 16,383 == 1111 1111  0111 1111 (0xFF 0x7F)
varLenQuantity :: Int -> [Word8]
varLenQuantity n
  | n < 0     = undefined -- fix
  | n < 2^7   = pack7Bits n
  | n < 2^14  = multiByte $ pack14Bits n
  | n < 2^21  = multiByte $ pack21Bits n
  | n < 2^28  = multiByte $ pack28Bits n
  | otherwise = undefined -- fix

-- sets the high bit of the first byte (signifies multibyte)
multiByte :: [Word8] -> [Word8]
multiByte (x:xs) = setBit x 7 : xs

mask :: Word8
mask = fromIntegral 0x7f

pack7Bits :: Int -> [Word8]
pack7Bits n = [fromIntegral n .&. mask]            -- n & 0x7f (01111111b)

pack14Bits :: Int -> [Word8]
pack14Bits n = fromIntegral (shiftR n 7) .&. mask  -- (n >> 7) & 0x7f
             : pack7Bits n

pack21Bits :: Int -> [Word8]
pack21Bits n = fromIntegral (shiftR n 14) .&. mask -- (n >> 14) & 0x7f
             : pack14Bits n
            ++ pack7Bits n

pack28Bits :: Int -> [Word8]
pack28Bits n = fromIntegral (shiftR n 21) .&. mask -- (n >> 21) & 0x7f
             : pack21Bits n
            ++ pack14Bits n
            ++ pack7Bits n

writeMidiHeader :: Put
writeMidiHeader = do
  putByteString "MThd" -- chunk id, always this
  putWord32be   6      -- chunk size (3 Word16s)
  putWord16be   0      -- midi format (zero is easy)
  putWord16be   1      -- number of tracks
  putWord16be   480    -- time division (480 ticks/quarter note)

writeTrackChunk :: Word32 -> Put
writeTrackChunk len = do
  putByteString "MTrk"
  putWord32be   len    -- chunk size

{- Set tempo

    [00]  [ff]  [51]  [03]  [07 a1 20]
    time  meta  tempo size  500k=120bpm
-}
setTempoEvent :: Put
setTempoEvent = do
  putWord8 0x00        -- time (0)
  putWord8 0xff        -- meta event
  putWord8 0x51        -- we're setting tempo
  putWord8 0x03        -- size of arguments (3)
  putWord8 0x07        -- tempo (in µs/quarter note)
  putWord8 0xa1
  putWord8 0x20        -- TODO: 3-byte nums (500,000)

{- Key signature

    [00]  [ff]  [59]  [02]  [00]  [00]
    time  meta  ksig  size  key   scal
                            (C)   (maj)
-}
setKeySignatureEvent :: Put
setKeySignatureEvent = do
  putWord8 0x00        -- time (0)
  putWord8 0xff        -- meta event
  putWord8 0x59        -- key signature event
  putWord8 0x02        -- size of arguments
  putWord8 0x00        -- key in num of sharps+/flats- (C)
  putWord8 0x00        -- scale 0=major

{- Time signature

    [00]  [ff]  [58]  [04]  [04]  [02]  [30]  [08]
    time  meta  tsig  size  numer denom metro 32nds
                            (4)  (2^2=4) (48) (8/32 in q.note)
-}
setTimeSignatureEvent :: Put
setTimeSignatureEvent = do
  putWord8 0x00        -- time (0)
  putWord8 0xff        -- meta event
  putWord8 0x58        -- time signature event
  putWord8 0x04        -- size of arguments
  putWord8 0x04        -- numerator (4)
  putWord8 0x02        -- denominator (as 2^x)
  putWord8 0x30        -- metronome (?)
  putWord8 0x08        -- num of 32nd notes to a beat

noteOnEvent :: Put
noteOnEvent = do
  putWord8 1           -- delta time (TODO: Variable Length Value)
  putWord8 0x90        -- 0x9_ = "Note on", 0x_0 = channel 0
  putWord8 60          -- middle C
  putWord8 105         -- max velocity

noteOffEvent :: Put
noteOffEvent = do
  putWord16be 0x816f   -- TODO: var length (239)
  putWord8 0x80        -- 0x8_ = "Note off", 0x_0 = channel 0
  putWord8 60          -- middle C
  putWord8 0           -- max velocity

{- End of track

   [1a]  [ff]  [2f]  [00]
   time  meta  eot   size
   (26)
-}
trackEndEvent :: Put
trackEndEvent = do
  putWord8 0x1a        -- time (26)
  putWord8 0xff        -- meta event
  putWord8 0x2f        -- end of track
  putWord8 0x00        -- size

midiFile = do
  writeMidiHeader
  writeTrackChunk 34    -- sizes of following events:
  setTempoEvent         -- size 7
  setKeySignatureEvent  -- size 6
  setTimeSignatureEvent -- size 8
  noteOnEvent           -- size 4
  noteOffEvent          -- size 5
  trackEndEvent         -- size 4

main = BL.putStr $ runPut midiFile
