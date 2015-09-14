module Sound.Midi.Events where

import Control.Monad.Free (liftF)
import Data.ByteString (ByteString)

import Sound.Midi.Internal.Types

noteOff :: Note -> Velocity -> Float -> TrackM ()
noteOff note vel beats = liftF $ VoiceChunk beats (NoteOff note vel) ()

noteOn :: Note -> Velocity -> Float -> TrackM ()
noteOn note vel beats = liftF $ VoiceChunk beats (NoteOn note vel) ()

afterTouch :: Note -> Pressure -> Float -> TrackM ()
afterTouch note pres beats = liftF $ VoiceChunk beats (AfterTouch note pres) ()

controlChange :: ControllerIdent -> ControllerValue -> Float -> TrackM ()
controlChange ident value beats = liftF $ VoiceChunk beats (ControlChange ident value) ()

patchChange :: Patch -> Float -> TrackM ()
patchChange patch beats = liftF $ VoiceChunk beats (PatchChange patch) ()

channelPressure :: Pressure -> Float -> TrackM ()
channelPressure pressure beats = liftF $ VoiceChunk beats (ChannelPressure pressure) ()

pitchWheelChange :: PitchWheel -> Float -> TrackM ()
pitchWheelChange pitch beats = liftF $ VoiceChunk beats (PitchWheelChange pitch) ()

sequenceNumber :: Sequence -> Float -> TrackM ()
sequenceNumber num beats = liftF $ MetaChunk beats (SequenceNumber num) ()

textArbitrary :: ByteString -> Float -> TrackM ()
textArbitrary text beats = liftF $ MetaChunk beats (TextArbitrary text) ()

textCopyright :: ByteString -> Float -> TrackM ()
textCopyright text beats = liftF $ MetaChunk beats (TextCopyright text) ()

textTrackName :: ByteString -> Float -> TrackM ()
textTrackName text beats = liftF $ MetaChunk beats (TextTrackName text) ()

textInstruName :: ByteString -> Float -> TrackM ()
textInstruName text beats = liftF $ MetaChunk beats (TextInstruName text) ()

textLyric :: ByteString -> Float -> TrackM ()
textLyric text beats = liftF $ MetaChunk beats (TextLyric text) ()

textMarker :: ByteString -> Float -> TrackM ()
textMarker text beats = liftF $ MetaChunk beats (TextMarker text) ()

textCuePoint :: ByteString -> Float -> TrackM ()
textCuePoint text beats = liftF $ MetaChunk beats (TextCuePoint text) ()

setTempo :: Tempo -> Float -> TrackM ()
setTempo tempo beats = liftF $ MetaChunk beats (SetTempo tempo) ()

-- SetTimeSig -- gotta figure this out

setKeySig :: KeySignature -> Float -> TrackM ()
setKeySig keySig beats = liftF $ MetaChunk beats (SetKeySig keySig) ()
