module Sound.Midi.Events where

import Control.Monad.Free (liftF)
import Data.ByteString (ByteString)
import Sound.Midi.Internal.Encoding (ChunkM (..), Track, delay)
import Sound.Midi.Internal.Encoding.Event (MetaChunk (..), VoiceChunk (..))

import Sound.Midi.Values

-- modifiers

rest :: Float -> Track -> Track
rest = delay

-- voice events

noteOff :: Note -> Velocity -> Track
noteOff note vel = liftF $ VoiceChunk 0 (NoteOff note vel) ()

noteOn :: Note -> Velocity -> Track
noteOn note vel = liftF $ VoiceChunk 0 (NoteOn note vel) ()

afterTouch :: Note -> Pressure -> Track
afterTouch note pres = liftF $ VoiceChunk 0 (AfterTouch note pres) ()

controller :: ControllerIdent -> ControllerValue -> Track
controller ident value = liftF $ VoiceChunk 0 (ControlChange ident value) ()

patch :: Patch -> Track
patch p = liftF $ VoiceChunk 0 (PatchChange p) ()

channelPressure :: Pressure -> Track
channelPressure pres = liftF $ VoiceChunk 0 (ChannelPressure pres) ()

pitchWheel :: PitchWheel -> Track
pitchWheel pitch = liftF $ VoiceChunk 0 (PitchWheelChange pitch) ()

-- meta events

sequence :: Sequence -> Track
sequence num = liftF $ MetaChunk 0 (SequenceNumber num) ()

text :: ByteString -> Track
text str = liftF $ MetaChunk 0 (TextArbitrary str) ()

copyright :: ByteString -> Track
copyright str = liftF $ MetaChunk 0 (TextCopyright str) ()

trackName :: ByteString -> Track
trackName str = liftF $ MetaChunk 0 (TextTrackName str) ()

instrumentName :: ByteString -> Track
instrumentName str = liftF $ MetaChunk 0 (TextInstruName str) ()

lyric :: ByteString -> Track
lyric str = liftF $ MetaChunk 0 (TextLyric str) ()

marker :: ByteString -> Track
marker str = liftF $ MetaChunk 0 (TextMarker str) ()

cuePoint :: ByteString -> Track
cuePoint str = liftF $ MetaChunk 0 (TextCuePoint str) ()

tempo :: Tempo -> Track
tempo t = liftF $ MetaChunk 0 (SetTempo t) ()

keySig :: KeySignature -> Track
keySig sig = liftF $ MetaChunk 0 (SetKeySig sig) ()
