module Sound.Midi.Events where

import Control.Monad.Free (liftF)
import Data.ByteString (ByteString)
import Sound.Midi.Internal.Encoding (ChunkM (..), Track)
import Sound.Midi.Internal.Encoding.Event (MetaChunk (..), VoiceChunk (..))

import Sound.Midi.Values

noteOff :: Note -> Velocity -> Float -> Track
noteOff note vel beats = liftF $ VoiceChunk beats (NoteOff note vel) ()

noteOn :: Note -> Velocity -> Float -> Track
noteOn note vel beats = liftF $ VoiceChunk beats (NoteOn note vel) ()

afterTouch :: Note -> Pressure -> Float -> Track
afterTouch note pres beats = liftF $ VoiceChunk beats (AfterTouch note pres) ()

controlChange :: ControllerIdent -> ControllerValue -> Float -> Track
controlChange ident value beats = liftF $ VoiceChunk beats (ControlChange ident value) ()

patchChange :: Patch -> Float -> Track
patchChange patch beats = liftF $ VoiceChunk beats (PatchChange patch) ()

channelPressure :: Pressure -> Float -> Track
channelPressure pres beats = liftF $ VoiceChunk beats (ChannelPressure pres) ()

pitchWheelChange :: PitchWheel -> Float -> Track
pitchWheelChange pitch beats = liftF $ VoiceChunk beats (PitchWheelChange pitch) ()

sequenceNumber :: Sequence -> Float -> Track
sequenceNumber num beats = liftF $ MetaChunk beats (SequenceNumber num) ()

textArbitrary :: ByteString -> Float -> Track
textArbitrary text beats = liftF $ MetaChunk beats (TextArbitrary text) ()

textCopyright :: ByteString -> Float -> Track
textCopyright text beats = liftF $ MetaChunk beats (TextCopyright text) ()

textTrackName :: ByteString -> Float -> Track
textTrackName text beats = liftF $ MetaChunk beats (TextTrackName text) ()

textInstruName :: ByteString -> Float -> Track
textInstruName text beats = liftF $ MetaChunk beats (TextInstruName text) ()

textLyric :: ByteString -> Float -> Track
textLyric text beats = liftF $ MetaChunk beats (TextLyric text) ()

textMarker :: ByteString -> Float -> Track
textMarker text beats = liftF $ MetaChunk beats (TextMarker text) ()

textCuePoint :: ByteString -> Float -> Track
textCuePoint text beats = liftF $ MetaChunk beats (TextCuePoint text) ()

setTempo :: Tempo -> Float -> Track
setTempo tempo beats = liftF $ MetaChunk beats (SetTempo tempo) ()

-- SetTimeSig -- gotta figure this out

setKeySig :: KeySignature -> Float -> Track
setKeySig keySig beats = liftF $ MetaChunk beats (SetKeySig keySig) ()
