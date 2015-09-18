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

import Sound.Midi.Internal.Ops (correlateRanges)
import Sound.Midi.Internal.Encoding.Value (PitchWheel (..))

highest, higher, high, centered, low, lower, lowest :: PitchWheel

highest   = pitchWheel 100
higher    = pitchWheel 66.6
high      = pitchWheel 33.3
centered  = pitchWheel 0
low       = pitchWheel (-33.3)
lower     = pitchWheel (-66.6)
lowest    = pitchWheel (-100)

pitchWheel :: Float -> PitchWheel
pitchWheel = PitchWheel . fromIntegral . correlateRanges (-100,100) (0,16383)
