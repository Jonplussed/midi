module Sound.MIDI.Types where

import Sound.MIDI.Internal.Types

data FileFormat
  = SingleTrack
  | SynchMultiTrack
  | AsynchMultiTrack
  deriving (Eq, Show)

instance ToWord8 FileFormat where
  toWord8 ff = case ff of
    SingleTrack       -> 0
    SynchMultiTrack   -> 1
    AsynchMultiTrack  -> 2

data Event
  = NoteOff
  | NoteOn
  | KeyAfterTouch
  | ControlChange
  | PatchChange
  | ChannelAfterTouch
  | PitchWheelChange
  deriving (Eq, Show)

instance ToWord8 Event where
  toWord8 event = case event of
    NoteOff           -> 0x80
    NoteOn            -> 0x90
    KeyAfterTouch     -> 0xA0
    ControlChange     -> 0xB0
    PatchChange       -> 0xC0
    ChannelAfterTouch -> 0xD0
    PitchWheelChange  -> 0xE0
