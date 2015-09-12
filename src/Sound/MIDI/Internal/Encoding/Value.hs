module Sound.Midi.Internal.Encoding.Value where

import Data.Bits (Bits, (.&.), (.|.), shiftR)
import Data.ByteString (ByteString)
import Data.Monoid ((<>))
import Data.Word (Word8, Word16, Word32)

import qualified Data.ByteString.Lazy.Builder as Bld

import Sound.Midi.Internal.Types

instance Encodable ControllerIdent where
  encode (ControllerIdent ci) = Bld.word8 ci

instance Encodable ControllerValue where
  encode (ControllerValue cv) = Bld.word8 cv

instance Encodable DeltaTime where
  encode (DeltaTime dt) = foldMap Bld.word8 $ encodeVarLength dt

instance Encodable FileFormat where
  encode (FileFormat ff) = Bld.word16BE ff

instance Encodable KeySignature where
  encode (KeySignature (KeyNote note, KeyChord chord)) =
    Bld.word8 note <>
    Bld.word8 chord

instance Encodable Note where
  encode (Note note) = Bld.word8 note

instance Encodable Patch where
  encode (Patch patch) = Bld.word8 patch

instance Encodable PitchWheel where
  encode (PitchWheel pitch) =
      word8' pitch <>
      word8' (drop7Bits pitch)
    where
      word8' = Bld.word8 . fromIntegral . mask7Bits

instance Encodable PPQN where
  encode (PPQN ppqn) = Bld.word16BE ppqn

instance Encodable Pressure where
  encode (Pressure pres) = Bld.word8 pres

instance Encodable Sequence where
  encode (Sequence seq) = Bld.word16BE seq

instance Encodable TrackCount where
  encode (TrackCount tc) = Bld.word16BE tc

instance Encodable Velocity where
  encode (Velocity vel) = Bld.word8 vel

-- private functions

drop7Bits :: (Bits a, Num a) => a -> a
drop7Bits n = shiftR n 7

mask7Bits :: (Bits a, Num a) => a -> a
mask7Bits n = 127 .&. n

on8thBit :: (Bits a, Num a) => a -> a
on8thBit n = 128 .|. n

-- a value of with bits:  aaaabbbbccccdddd
-- is represented as:     100000aa 1aabbbbc 0cccdddd
encodeVarLength :: (Bits a, Integral a) => a -> [Word8]
encodeVarLength = go [] id
  where
    go :: (Bits a, Integral a) => [Word8] -> (a -> a) -> a -> [Word8]
    go words nextValMask val =
        if remaining > 0
        then go (nextVal : words) on8thBit remaining
        else words
      where
        nextVal = fromIntegral . nextValMask $ mask7Bits val
        remaining = drop7Bits val
