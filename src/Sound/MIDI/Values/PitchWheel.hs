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

import Data.Bits ((.|.), shiftL, shiftR)
import Data.Word (Word16)
import Sound.Midi.Internal.Ops (correlateRanges, mask7Bits)
import Sound.Midi.Internal.Types (PitchWheel (..))

highest   = pitchWheel 100
higher    = pitchWheel 66.6
high      = pitchWheel 33.
centered  = pitchWheel 0
low       = pitchWheel (-33.3)
lower     = pitchWheel (-66.6)
lowest    = pitchWheel (-100)

pitchWheel :: Float -> PitchWheel
pitchWheel 0 = centered
pitchWheel n = PitchWheel . splitBytes $ correlateRanges (-100,100) (0,16383) n

-- the PitchWheel value is is a 14-bit uint represented by two 8-bit uints,
-- where the first byte's bits 0-6 are the value's bits 0-6 and the second
-- byte's bits 0-6 are the value's bits 7-13
splitBytes :: Int -> Word16
splitBytes n = fromIntegral $ shiftL (mask7Bits n) 8 .|. mask7Bits (shiftR n 7)
