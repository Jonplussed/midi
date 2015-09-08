module Sound.Midi.Values.PitchWheel
( highest
, higher
, high
, centered
, low
, lower
, lowest
, percent
) where

import Data.Bits (Bits, (.|.), shiftL, shiftR)
import Data.Word (Word16)
import Sound.Midi.Internal.Ops (correlateRanges, mask7Bits)
import Sound.Midi.Internal.Types (PitchWheel (..))

highest   = percent 100
higher    = percent 66.6
high      = percent 33.3
centered  = percent 0
low       = percent (-33.3)
lower     = percent (-66.6)
lowest    = percent (-100)

percent :: RealFrac a => a -> PitchWheel
percent = PitchWheel . pack2Bytes . correlateRanges (-100,100) (0 :: Int, 16383)

-- private functions

-- the PitchWheel value is is a 14-bit uint represented by two 8-bit uints,
-- where the first byte's bits 0-6 are the value's bits 0-6 and the second
-- byte's bits 0-6 are the value's bits 7-13
pack2Bytes :: (Bits a, Integral a) => a -> Word16
pack2Bytes n = fromIntegral $ shiftL (mask7Bits n) 8 .|. mask7Bits (shiftR n 7)
