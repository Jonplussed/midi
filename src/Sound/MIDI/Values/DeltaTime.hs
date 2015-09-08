module Sound.Midi.Values.DeltaTime where

import Data.Bits (Bits)
import Sound.Midi.Internal.Ops (encodeVarLength)
import Sound.Midi.Internal.Types (DeltaTime (..))

deltaTime :: (Bits a, Integral a) => a -> DeltaTime
deltaTime = DeltaTime . encodeVarLength
