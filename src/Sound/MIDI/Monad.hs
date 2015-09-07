module Sound.Midi.Monad where

import Control.Monad.Free
import Data.Binary.Put
import Sound.Midi.Internal.Types
import Sound.Midi.Values.VoiceEvent

data TrackF next
  = NoteOff DeltaTime Note Velocity next
  | NoteOn DeltaTime Note Velocity next
  | AfterTouch DeltaTime Note Pressure next
  | ControlChange DeltaTime ControllerIdent ControllerValue next
  | PatchChange DeltaTime Patch next
  | ChannelPressure DeltaTime Pressure next
  | PitchWheelChange DeltaTime next

type Track a = Free TrackF a

runTrack :: Channel -> Track a -> Put
runTrack chan = interp
  where
    interp (Free (NoteOff dt (Note note) (Velocity vel) next)) = do
      putDeltaTime dt
      putEvent noteOff chan
      putWord8 note
      putWord8 vel
      interp next
    interp (Free (NoteOn dt (Note note) (Velocity vel) next)) = do
      putDeltaTime dt
      putEvent noteOn chan
      putWord8 note
      putWord8 vel
      interp next
    interp (Free (AfterTouch dt (Note note) (Pressure pres) next)) = do
      putDeltaTime dt
      putEvent afterTouch chan
      putWord8 note
      putWord8 pres
      interp next

putDeltaTime :: DeltaTime -> Put
putDeltaTime (DeltaTime words) = mapM_ putWord8 words

putEvent :: VoiceEvent -> Channel -> Put
putEvent (VoiceEvent ve) = putWord8 . ve
