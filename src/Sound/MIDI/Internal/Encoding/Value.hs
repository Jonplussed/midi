module Sound.Midi.Internal.Encoding.Value
( Beats (..)
, Channel (..)
, ControllerIdent (..)
, ControllerValue (..)
, DeltaTime (..)
, FileFormat (..)
, KeyChord (..)
, KeyNote (..)
, KeySignature (..)
, Note (..)
, Patch (..)
, PitchWheel (..)
, PPQN (..)
, Pressure (..)
, Sequence (..)
, Tempo (..)
, TrackCount (..)
, Velocity (..)
, Encodable (..)
) where

import Data.Bits (Bits, (.&.), (.|.), shiftR)
import Data.Monoid ((<>))
import Data.Word (Word8, Word16)
import Data.Word.Word24 (Word24)

import qualified Data.ByteString.Lazy.Builder as Bld

newtype Beats           = Beats Float                       deriving (Show)
newtype Channel         = Channel Word8                     deriving (Show)
newtype ControllerIdent = ControllerIdent Word8             deriving (Show)
newtype ControllerValue = ControllerValue Word8             deriving (Show)
newtype DeltaTime       = DeltaTime Int                     deriving (Show)
newtype FileFormat      = FileFormat Word16                 deriving (Show)
newtype KeyChord        = KeyChord Word8                    deriving (Show)
newtype KeyNote         = KeyNote Word8                     deriving (Show)
newtype KeySignature    = KeySignature (KeyNote, KeyChord)  deriving (Show)
newtype Note            = Note Word8                        deriving (Show)
newtype Patch           = Patch Word8                       deriving (Show)
newtype PitchWheel      = PitchWheel Word16                 deriving (Show)
newtype PPQN            = PPQN Word16                       deriving (Show)
newtype Pressure        = Pressure Word8                    deriving (Show)
newtype Sequence        = Sequence Word16                   deriving (Show)
newtype Tempo           = Tempo Word24                      deriving (Show)
newtype TrackCount      = TrackCount Word16                 deriving (Show)
newtype Velocity        = Velocity Word8                    deriving (Show)

class Encodable a where
  encode :: a -> Bld.Builder

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
  encode (Sequence num) = Bld.word16BE num

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
    go segments nextValMask val =
        if remaining > 0
        then go (nextVal : segments) on8thBit remaining
        else segments
      where
        nextVal = fromIntegral . nextValMask $ mask7Bits val
        remaining = drop7Bits val
