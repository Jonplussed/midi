module Sound.Midi.Values.Velocity where

import Sound.Midi.Internal.Ops (correlateRanges)
import Sound.Midi.Internal.Encoding.Value (Velocity (..))

fastest   = velocity 100
faster    = velocity 80
fast      = velocity 60
moderate  = velocity 50
slow      = velocity 40
slower    = velocity 20
slowest   = velocity 1

velocity :: RealFrac a => a -> Velocity
velocity = Velocity . fromIntegral . correlateRanges (0,100) (0,127)
