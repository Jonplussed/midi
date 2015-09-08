module Sound.Midi.Values.Velocity where

import Sound.Midi.Internal.Ops (correlateRanges)
import Sound.Midi.Internal.Types (Velocity (..))

fastest = percent 100
faster  = percent 80
fast    = percent 60
slow    = percent 40
slower  = percent 20
slowest = percent 1

percent :: RealFrac a => a -> Velocity
percent = Velocity . fromIntegral . correlateRanges (0,100) (0,127)
