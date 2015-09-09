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
percent = PitchWheel . fromIntegral . correlateRanges (-100,100) (0 :: Int, 16383)
