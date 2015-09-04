module Sound.Midi.Values.PitchWheel
( highest
, higher
, high
, centered
, low
, lower
, lowest
, pitchWheel
) where

import Data.Bits ((.|.), shiftR)
import Data.Word (Word16)
import Sound.Midi.Internal.Ops (correlateRanges, mask7Bits)
import Sound.Midi.Internal.Types (PitchWheel (..))

highest   = splitBytes 16383
higher    = splitBytes 13652
high      = splitBytes 10921
centered  = splitBytes 8192
low       = splitBytes 5461
lower     = splitBytes 2730
lowest    = splitBytes 1

pitchWheel :: Float -> PitchWheel
pitchWheel 0 = centered
pitchWheel n = splitBytes $ correlateRanges (-100,100) (0,16383) n

-- the PitchWheel value is is a 14-bit uint represented by two 8-bit uints,
-- where the first byte's bits 0-6 are the value's bits 0-6 and the second
-- byte's bits 0-6 are the value's bits 7-13
splitBytes :: Int -> PitchWheel
splitBytes n = PitchWheel $ fromIntegral bits
  where
    bits = shift (mask7Bits n) 8 .|. mask7Bits (shiftR n 7)
