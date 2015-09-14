module Sound.Midi.Values.Pressure where

import Sound.Midi.Internal.Ops (correlateRanges)
import Sound.Midi.Internal.Types (Pressure (..))

hardest = pressure 100
harder  = pressure 80
hard    = pressure 60
soft    = pressure 40
softer  = pressure 20
softest = pressure 1

pressure :: RealFrac a => a -> Pressure
pressure = Pressure . fromIntegral . correlateRanges (0,100) (0,127)
