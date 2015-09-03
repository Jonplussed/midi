module Sound.Midi.Values.Velocity where

import Sound.Midi.Internal.Ops (percentWord8)
import Sound.Midi.Internal.Types (Velocity (..))

fastest = Velocity 127
faster  = Velocity 106
fast    = Velocity 85
slow    = Velocity 64
slower  = Velocity 43
slowest = Velocity 22

velocity :: Int -> Velocity
velocity = Velocity . percentWord8
