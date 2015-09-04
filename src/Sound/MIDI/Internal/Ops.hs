module Sound.Midi.Internal.Ops where

import Data.Word (Word8)

correlateRanges :: (Float, Float) -> (Int, Int) -> Float -> Int
correlateRanges from@(fromMin, fromMax) to@(toMin, toMax) val
    | val < fromMin = againWithVal fromMin
    | val > fromMax = againWithVal fromMax
    | otherwise = round $ (val - fromMin) / fromDiff * fromIntegral toDiff - fromIntegral toMin
  where
    againWithVal = correlateRanges from to
    fromDiff = fromMax - fromMin
    toDiff = toMax - toMin
