module Sound.Midi.Internal.Ops
( correlateRanges
, encodeVarLength
, mask7Bits
, on8thBit
) where

import Data.Binary.Put (Put, putWord8, runPut)
import Data.Bits (Bits, (.&.), (.|.), shiftR)
import Data.ByteString.Lazy (ByteString)
import Data.Word (Word8)

correlateRanges :: (RealFrac a, Integral b) => (a,a) -> (b,b) -> a -> b
correlateRanges from@(fromMin, fromMax) to@(toMin, toMax) val
    | val < fromMin = correlateRanges from to fromMin
    | val > fromMax = correlateRanges from to fromMax
    | otherwise = round $ (val - fromMin) / fromDiff * fromIntegral toDiff - fromIntegral toMin
  where
    fromDiff = fromMax - fromMin
    toDiff = toMax - toMin

encodeVarLength :: Int -> ByteString
encodeVarLength = runPut . encodeVarLength'

mask7Bits :: (Bits a , Num a) => a -> a
mask7Bits n = 127 .&. n

on8thBit :: (Bits a , Num a) => a -> a
on8thBit n = 128 .|. n

-- private functions

encodeVarLength' :: Int -> Put
encodeVarLength' val
    | remaining == 0 = putWord8 nextVal
    | otherwise = do
          putWord8 $ on8thBit nextVal
          encodeVarLength' remaining
  where
    remaining = shiftR val 7
    nextVal = fromIntegral $ mask7Bits val
