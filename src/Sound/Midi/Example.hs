{-# LANGUAGE OverloadedStrings #-}

module Sound.Midi.Example where

import Sound.Midi
import Sound.Midi.Events
import Sound.Midi.Values

example :: Midi
example = do

  track ch1 $ do
    trackName "lead melody"
    instrumentName "guitar"
    note aFlat4 2

  track ch2 $ do
    trackName "soft background melody"
    instrumentName "piano"
