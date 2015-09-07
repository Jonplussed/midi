module Sound.Midi.Monad
( Track
, TrackF (..)
, runTrack
) where

import Control.Monad.Free (Free (..))
import Data.Binary.Put (Put, putWord8, putWord16be)

import qualified Sound.Midi.Values.VoiceEvent as VE

import Sound.Midi.Internal.Types

data TrackF next
  = NoteOff DeltaTime Note Velocity next
  | NoteOn DeltaTime Note Velocity next
  | AfterTouch DeltaTime Note Pressure next
  | ControlChange DeltaTime ControllerIdent ControllerValue next
  | PatchChange DeltaTime Patch next
  | ChannelPressure DeltaTime Pressure next
  | PitchWheelChange DeltaTime PitchWheel next

type Track a = Free TrackF a

runTrack :: Channel -> Track a -> Put
runTrack chan = interp
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

-- private functions

putDeltaTime :: DeltaTime -> Put
putDeltaTime (DeltaTime words) = mapM_ putWord8 words

putEvent :: VoiceEvent -> Channel -> Put
putEvent (VoiceEvent withChan) = putWord8 . withChan
