module Sound.Midi.Values.DeltaTime where

import Data.Bits (Bits)
import Sound.Midi.Internal.Ops (encodeVarLength)
import Sound.Midi.Internal.Types (DeltaTime (..), PPQN (..))

newtype DeltaTime = DeltaTime Int

pulses :: Int -> DeltaTime
pulses = deltaTime

beats :: PPQN -> Int -> DeltaTime
beats (PPQN ppqn) count = deltaTime $ fromIntegral ppqn * count

-- private functions

deltaTime :: (Bits a, Integral a) => a -> DeltaTime
deltaTime = DeltaTime . encodeVarLength
