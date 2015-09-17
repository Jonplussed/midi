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

data VoiceChunk
  = NoteOff Note Velocity
  | NoteOn Note Velocity
  | AfterTouch Note Pressure
  | ControlChange ControllerIdent ControllerValue
  | PatchChange Patch
  | ChannelPressure Pressure
  | PitchWheelChange PitchWheel
  deriving (Show)

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
  deriving (Show)

data ChunkM next
  = VoiceChunk Float VoiceChunk next
  | MetaChunk Float MetaChunk next
  | Rest Float next
  deriving (Show, Functor)

type MidiM = ReaderT PPQN (State (TrackCount, Bld.Builder))
type Midi = MidiM ()

type TrackM = Free ChunkM
type Track = TrackM ()

class Encodable a where
  encode :: a -> Bld.Builder
