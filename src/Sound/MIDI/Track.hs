{-# LANGUAGE OverloadedStrings #-}

module Sound.Midi.Track
( TrackM
, ChunkM (..)
, runTrack
) where

import Control.Monad.Free (Free (..))

import qualified Data.ByteString.Lazy as LazyBS
import qualified Data.ByteString as BS
import qualified Sound.Midi.Values.VoiceEvent as VE

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
  | TrackEnd DeltaTime

type TrackM a = Free ChunkM a

runTrack :: Channel -> TrackM a -> Put
runTrack chan = trackStart . interp
  where
    interp (Free (NoteOff dt (Note note) (Velocity vel) next)) = do
      putDeltaTime dt
      putEvent VE.noteOff chan
      putWord8 note
      putWord8 vel
      interp next
    interp (Free (NoteOn dt (Note note) (Velocity vel) next)) = do
      putDeltaTime dt
      putEvent VE.noteOn chan
      putWord8 note
      putWord8 vel
      interp next
    interp (Free (AfterTouch dt (Note note) (Pressure pres) next)) = do
      putDeltaTime dt
      putEvent VE.afterTouch chan
      putWord8 note
      putWord8 pres
      interp next
    interp (Free (ControlChange dt (ControllerIdent ident) (ControllerValue val) next)) = do
      putDeltaTime dt
      putEvent VE.controlChange chan
      putWord8 ident
      putWord8 val
      interp next
    interp (Free (PatchChange dt (Patch patch) next)) = do
      putDeltaTime dt
      putEvent VE.patchChange chan
      putWord8 patch
      interp next
    interp (Free (ChannelPressure dt (Pressure pres) next)) = do
      putDeltaTime dt
      putEvent VE.channelPressure chan
      putWord8 pres
      interp next
    interp (Free (PitchWheelChange dt (PitchWheel pitch) next)) = do
      putDeltaTime dt
      putEvent VE.pitchWheelChange chan
      putWord16be pitch
      interp next
    interp (Free (TrackEnd dt)) = do
      putDeltaTime dt
      trackEnd

-- private functions

trackStart :: Put -> Put
trackStart put = do
    putByteString "MTrk"
    putWord32be . fromIntegral $ BS.length str
    putByteString str
  where
    str = LazyBS.toStrict $ runPut put

trackEnd :: Put
trackEnd = do
    putWord8 0xff
    putWord8 0x2f
    putWord8 0x00

putDeltaTime :: DeltaTime -> Put
putDeltaTime (DeltaTime words) = mapM_ putWord8 words

putEvent :: VoiceEvent -> Channel -> Put
putEvent (VoiceEvent withChan) = putWord8 . withChan
