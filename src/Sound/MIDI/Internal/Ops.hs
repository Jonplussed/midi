module Sound.Midi.Internal.Ops
( correlateRanges
, encodeVarLength
, mask7Bits
, on8thBit
) where

import Data.Bits (Bits, (.&.), (.|.), shiftR)
import Data.ByteString.Lazy (ByteString)
import Data.Word (Word, Word8)

correlateRanges :: (RealFrac a, Integral b) => (a,a) -> (b,b) -> a -> b
correlateRanges from@(fromMin, fromMax) to@(toMin, toMax) val
    | val < fromMin = correlateRanges from to fromMin
    | val > fromMax = correlateRanges from to fromMax
    | otherwise = round $ (val - fromMin) / fromDiff * fromIntegral toDiff - fromIntegral toMin
  where
    fromDiff = fromMax - fromMin
    toDiff = toMax - toMin

mask7Bits :: (Bits a, Num a) => a -> a
mask7Bits n = 127 .&. n

on8thBit :: (Bits a, Num a) => a -> a
on8thBit n = 128 .|. n

encodeVarLength :: (Bits a, Integral a) => a -> [Word8]
encodeVarLength = encodeVarLength' [] id

-- private functions

-- a value of with bits:  aaaabbbbccccdddd
-- is represented as:     100000aa 1aabbbbc 0cccdddd
encodeVarLength' :: (Bits a, Integral a) => [Word8] -> (a -> a) -> a -> [Word8]
encodeVarLength' words nextValMask val =
    if remaining > 0
    then encodeVarLength' (nextVal : words) on8thBit remaining
    else words
  where
    nextVal = fromIntegral . nextValMask $ mask7Bits val
    remaining = shiftR val 7
