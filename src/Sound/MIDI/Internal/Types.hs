module Sound.Midi.Internal.Types where

import Control.Monad.Free (Free (..))
import Data.Binary.Put (Put, putWord8)
import Data.ByteString (ByteString)
import Data.Word (Word8, Word16, Word32)
import Data.Word.Word24 (Word24)

class Encodable a where
  encode :: a -> Put

newtype Channel         = Channel Word8
newtype ControllerIdent = ControllerIdent Word8
newtype ControllerValue = ControllerValue Word8
newtype DeltaTime       = DeltaTime Int
newtype FileFormat      = FileFormat Word16
newtype KeyChord        = KeyChord Word8
newtype KeyNote         = KeyNote Word8
newtype KeySignature    = KeySignature (KeyNote, KeyChord)
newtype Note            = Note Word8
newtype Patch           = Patch Word8
newtype PitchWheel      = PitchWheel Word16
newtype PPQN            = PPQN Word16
newtype Pressure        = Pressure Word8
newtype Sequence        = Sequence Word16
newtype Tempo           = Tempo Word24
newtype Velocity        = Velocity Word8

data VoiceChunk
  = NoteOff Note Velocity
  | NoteOn Note Velocity
  | AfterTouch Note Pressure
  | ControlChange ControllerIdent ControllerValue
  | PatchChange Patch
  | ChannelPressure Pressure
  | PitchWheelChange PitchWheel

-- still needs Tempo and TimeSig
data MetaChunk
  = SequenceNumber Sequence
  | TextArbitrary ByteString
  | TextCopyright ByteString
  | TextTrackName ByteString
  | TextInstruName ByteString
  | TextLyric ByteString
  | TextMarker ByteString
  | TextCuePoint ByteString
  | SetTempo Tempo
  | SetTimeSig -- gotta figure this out
  | SetKeySig KeySignature
  | EndOfTrack

data ChunkM next
  = VoiceChunk DeltaTime VoiceChunk next
  | MetaChunk DeltaTime MetaChunk next
  | Rest DeltaTime next

type TrackM next = Free ChunkM next
