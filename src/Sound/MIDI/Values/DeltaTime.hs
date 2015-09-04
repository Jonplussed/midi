module Sound.Midi.Values.DeltaTime where

import Sound.Midi.Internal.Ops (encodeVarLength)
import Sound.Midi.Internal.Types (DeltaTime (..))

deltaTime :: Int -> DeltaTime
deltaTime = DeltaTime . encodeVarLength
