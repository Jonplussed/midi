module Sound.Midi.Internal.Encoding.Value where

import Data.Bits (Bits, (.&.), (.|.), shiftR)
import Data.ByteString (ByteString)
import Data.Word (Word8, Word16, Word32)

import qualified Data.Binary.Put as Put

import Sound.Midi.Internal.Types

instance Encodable ControllerIdent where
  encode (ControllerIdent ci) = Put.putWord8 ci

instance Encodable ControllerValue where
  encode (ControllerValue cv) = Put.putWord8 cv

instance Encodable DeltaTime where
  encode (DeltaTime dt) = mapM_ Put.putWord8 $ encodeVarLength dt

instance Encodable FileFormat where
  encode (FileFormat ff) = Put.putWord16be ff

instance Encodable KeySignature where
  encode (KeySignature (KeyNote note, KeyChord chord)) = do
    Put.putWord8 note
    Put.putWord8 chord

instance Encodable Note where
  encode (Note note) = Put.putWord8 note

instance Encodable Patch where
  encode (Patch patch) = Put.putWord8 patch

instance Encodable PitchWheel where
  encode (PitchWheel pitch) = do
    let word8' = Put.putWord8 . fromIntegral . mask7Bits
    word8' pitch
    word8' (drop7Bits pitch)

instance Encodable PPQN where
  encode (PPQN ppqn) = Put.putWord16be ppqn

instance Encodable Pressure where
  encode (Pressure pres) = Put.putWord8 pres

instance Encodable Sequence where
  encode (Sequence seq) = Put.putWord16be seq

instance Encodable Tempo where
  encode (Tempo tempo) = undefined

instance Encodable TrackCount where
  encode (TrackCount tc) = Put.putWord16be tc

instance Encodable Velocity where
  encode (Velocity vel) = Put.putWord8 vel

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
