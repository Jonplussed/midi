{-# LANGUAGE OverloadedStrings #-}

module Sound.Midi.Track
( TrackM
, ChunkM (..)
, putTrack
) where

import Control.Monad.Free (Free (..))

import qualified Data.ByteString.Lazy as LazyBS
import qualified Data.ByteString as StrictBS
import qualified Sound.Midi.Values.Event as Eve

import Data.Binary.Put
import Sound.Midi.Internal.Types

data ChunkM next
  = NoteOff DeltaTime Note Velocity next
  | NoteOn DeltaTime Note Velocity next
  | AfterTouch DeltaTime Note Pressure next
  | ControlChange DeltaTime ControllerIdent ControllerValue next
  | PatchChange DeltaTime Patch next
  | ChannelPressure DeltaTime Pressure next
  | PitchWheelChange DeltaTime PitchWheel next
  | SequenceNumber DeltaTime Sequence next
  | TextArbitrary DeltaTime StrictBS.ByteString next
  | TextCopyright DeltaTime StrictBS.ByteString next
  | TextTrackName DeltaTime StrictBS.ByteString next
  | TextInstruName DeltaTime StrictBS.ByteString next
  | TextLyric DeltaTime StrictBS.ByteString next
  | TextMarker DeltaTime StrictBS.ByteString next
  | TextCuePoint DeltaTime StrictBS.ByteString next
  | SetTempo DeltaTime next
  | SetTimeSig DeltaTime next
  | SetKeySig DeltaTime KeySignature next
  | EndOfTrack DeltaTime

type TrackM a = Free ChunkM a

putTrack :: Channel -> TrackM a -> Put
putTrack chan = putTrackHeader . interp
  where
    interp (Free (NoteOff dt (Note note) (Velocity vel) next)) = do
      putDeltaTime dt
      putVoiceEvent Eve.noteOff chan
      putWord8 note
      putWord8 vel
      interp next
    interp (Free (NoteOn dt (Note note) (Velocity vel) next)) = do
      putDeltaTime dt
      putVoiceEvent Eve.noteOn chan
      putWord8 note
      putWord8 vel
      interp next
    interp (Free (AfterTouch dt (Note note) (Pressure pres) next)) = do
      putDeltaTime dt
      putVoiceEvent Eve.afterTouch chan
      putWord8 note
      putWord8 pres
      interp next
    interp (Free (ControlChange dt (ControllerIdent ident) (ControllerValue val) next)) = do
      putDeltaTime dt
      putVoiceEvent Eve.controlChange chan
      putWord8 ident
      putWord8 val
      interp next
    interp (Free (PatchChange dt (Patch patch) next)) = do
      putDeltaTime dt
      putVoiceEvent Eve.patchChange chan
      putWord8 patch
      interp next
    interp (Free (ChannelPressure dt (Pressure pres) next)) = do
      putDeltaTime dt
      putVoiceEvent Eve.channelPressure chan
      putWord8 pres
      interp next
    interp (Free (PitchWheelChange dt (PitchWheel pitch) next)) = do
      putDeltaTime dt
      putVoiceEvent Eve.pitchWheelChange chan
      putWord16be pitch
      interp next
    interp (Free (SequenceNumber dt (Sequence num) next)) = do
      putDeltaTime dt
      putMetaEvent Eve.sequenceNumber
      putWord8 0x02
      putWord16be num
      interp next
    interp (Free (TextArbitrary dt text next)) = do
      putDeltaTime dt
      putMetaEvent Eve.textArbitrary
      putText text
      interp next
    interp (Free (TextCopyright dt text next)) = do
      putDeltaTime dt
      putMetaEvent Eve.textCopyright
      putText text
      interp next
    interp (Free (TextTrackName dt text next)) = do
      putDeltaTime dt
      putMetaEvent Eve.textTrackName
      putText text
      interp next
    interp (Free (TextInstruName dt text next)) = do
      putDeltaTime dt
      putMetaEvent Eve.textInstruName
      putText text
      interp next
    interp (Free (TextLyric dt text next)) = do
      putDeltaTime dt
      putMetaEvent Eve.textLyric
      putText text
      interp next
    interp (Free (TextMarker dt text next)) = do
      putDeltaTime dt
      putMetaEvent Eve.textMarker
      putText text
      interp next
    interp (Free (TextCuePoint dt text next)) = do
      putDeltaTime dt
      putMetaEvent Eve.textCuePoint
      putText text
      interp next
    -- interp (Free (SetTempo DeltaTime next
    -- interp (Free (SetTimeSig DeltaTime next
    interp (Free (SetKeySig dt (KeySignature keySig) next)) = do
      putDeltaTime dt
      putMetaEvent Eve.setKeySig
      putWord8 0x02
      putWord16be keySig
      interp next
    interp (Free (EndOfTrack dt)) = do
      putDeltaTime dt
      putMetaEvent Eve.endOfTrack
      putWord8 0x00

-- private functions

putTrackHeader :: Put -> Put
putTrackHeader put = do
    putByteString "MTrk"
    putWord32be . fromIntegral $ StrictBS.length str
    putByteString str
  where
    str = LazyBS.toStrict $ runPut put

putDeltaTime :: DeltaTime -> Put
putDeltaTime (DeltaTime words) = mapM_ putWord8 words

putMetaEvent :: MetaEvent -> Put
putMetaEvent (MetaEvent ident) = putWord16be ident

putVoiceEvent :: VoiceEvent -> Channel -> Put
putVoiceEvent (VoiceEvent withChan) = putWord8 . withChan

putText :: StrictBS.ByteString -> Put
putText bs = do
  putWord8 . fromIntegral $ StrictBS.length bs
  putByteString bs
