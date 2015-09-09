module Sound.Midi.Internal.Encoding.Value where

import Data.Binary.Put (Put, putByteString, putWord8, putWord16be)
import Data.Bits (Bits, (.&.), (.|.), shiftR)
import Data.Word (Word8)

import Sound.Midi.Internal.Types

instance Encodable ControllerIdent where
  encode (ControllerIdent ci) = putWord8 ci

instance Encodable ControllerValue where
  encode (ControllerValue cv) = putWord8 cv

instance Encodable DeltaTime where
  encode (DeltaTime dt) = mapM_ putWord8 $ encodeVarLength dt

instance Encodable KeySignature where
  encode (KeySignature (KeyNote note, KeyChord chord)) = do
    putWord8 note
    putWord8 chord

instance Encodable Note where
  encode (Note note) = putWord8 note

instance Encodable Patch where
  encode (Patch patch) = putWord8 patch

instance Encodable PitchWheel where
  encode (PitchWheel pitch) = do
    let putWord8' = putWord8 . fromIntegral . mask7Bits
    putWord8' pitch
    putWord8' (drop7Bits pitch)

instance Encodable Pressure where
  encode (Pressure pres) = putWord8 pres

instance Encodable Sequence where
  encode (Sequence seq) = putWord16be seq

instance Encodable Tempo where
  encode (Tempo tempo) = undefined

instance Encodable Velocity where
  encode (Velocity vel) = putWord8 vel

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
