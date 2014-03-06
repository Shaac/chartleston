module Midi where

import Sound.MIDI.File.Load (fromFile)
import Sound.MIDI.File
import Data.EventList.Relative.TimeBody (mapMaybe, getBodies)
import Sound.MIDI.File.Event (maybeMIDIEvent)
import Sound.MIDI.Message.Channel (messageBody, Body(Voice))
import Sound.MIDI.Message.Channel.Voice (T(NoteOn), fromPitch, fromVelocity)

data Music = Music {division :: Division, track :: [Note]}
    deriving (Show)

type Note = (Int, Int) -- Drum part, intensity.

open :: FilePath -> IO Music
open filename = do
    Cons t d tracks <- fromFile filename
    -- TODO Get meta events
    let notes = mapMaybe ((>>= return . messageBody) . maybeMIDIEvent) $ mergeTracks t tracks
    -- TODO keep time
    return $ Music d $ getBodies $ mapMaybe bodyToNote notes

bodyToNote :: Body -> Maybe Note
bodyToNote (Voice (NoteOn p v)) = Just (fromPitch p, fromVelocity v)
bodyToNote _                    = Nothing
