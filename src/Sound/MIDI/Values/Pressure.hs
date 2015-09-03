module Sound.Midi.Values.Pressure where

import Sound.Midi.Internal.Ops (percentWord8)
import Sound.Midi.Internal.Types (Pressure (..))

hardest = Pressure 127
harder  = Pressure 106
hard    = Pressure 85
soft    = Pressure 64
softer  = Pressure 43
softest = Pressure 22

pressure :: Int -> Pressure
pressure = Pressure . percentWord8
