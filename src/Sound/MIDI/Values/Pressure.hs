module Sound.Midi.Values.Pressure where

import Sound.Midi.Internal.Ops (correlateRanges)
import Sound.Midi.Internal.Types (Pressure (..))

hardest = percent 100
harder  = percent 80
hard    = percent 60
soft    = percent 40
softer  = percent 20
softest = percent 1

percent :: RealFrac a => a -> Pressure
percent = Pressure . fromIntegral . correlateRanges (0,100) (0,127)
