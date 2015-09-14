{-# LANGUAGE DeriveFunctor #-}

module Sound.Midi.Internal.Types where

import Control.Monad.Free (Free (..))
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (State)
import Data.Word (Word8, Word16, Word32)
import Data.Word.Word24 (Word24)

import qualified Data.ByteString as StrictBS
import qualified Data.ByteString.Lazy as LazyBS
import qualified Data.ByteString.Lazy.Builder as Bld

newtype Beats           = Beats Float
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
newtype TrackCount      = TrackCount Word16
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
  | TextArbitrary StrictBS.ByteString
  | TextCopyright StrictBS.ByteString
  | TextTrackName StrictBS.ByteString
  | TextInstruName StrictBS.ByteString
  | TextLyric StrictBS.ByteString
  | TextMarker StrictBS.ByteString
  | TextCuePoint StrictBS.ByteString
  | SetTempo Tempo
  | SetTimeSig -- gotta figure this out
  | SetKeySig KeySignature

data ChunkM next
  = VoiceChunk Float VoiceChunk next
  | MetaChunk Float MetaChunk next
  | Rest Float next
  deriving (Functor)

type TrackM = Free ChunkM
type Midi = ReaderT PPQN (State (TrackCount, Bld.Builder))

class Encodable a where
  encode :: a -> Bld.Builder
