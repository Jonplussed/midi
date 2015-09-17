{-# LANGUAGE OverloadedStrings #-}

module Sound.Midi.Example where

import Sound.Midi
import Sound.Midi.Events
import Sound.Midi.Values

example :: Midi
example = do
    track ch1  guitar
    track ch2  piano
    track ch3  drums
    track ch16 info

guitar :: Track
guitar = do
    instrumentName "guitar"

piano :: Track
piano = do
    instrumentName "piano"

drums :: Track
drums = do
    instrumentName "drums"

info :: Track
info = do
    keySig $ keyOf a minor
