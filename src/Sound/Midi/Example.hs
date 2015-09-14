module Sound.Midi.Example where

import Sound.Midi
import Sound.Midi.Events
import Sound.Midi.Values

example :: Midi
example = do
  track ch1 $ do
    instrumentName "guitar"
  track ch2 $ do
    instrumentName "piano"
