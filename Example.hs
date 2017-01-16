{-# LANGUAGE OverloadedStrings #-}

module Sound.Midi.Example where

import qualified Data.ByteString.Lazy as LazyBS

import Sound.Midi
import Sound.Midi.Events
import Sound.Midi.Values

main :: IO ()
main = LazyBS.putStr $ midi syncMultiTrack (ppqn 480) mySong

mySong :: Midi
mySong = do
    track ch16 $ do
        keySig a minor

    track ch1 $ do
        instrumentName "guitar"

    track ch2 $ do
        instrumentName "drums"
