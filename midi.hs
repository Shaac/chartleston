module Midi (open, Note) where

import Sound.MIDI.File.Load (fromFile)
import Sound.MIDI.File
import Data.EventList.Relative.TimeBody (mapMaybe, mapTime, toPairList)
import Sound.MIDI.File.Event (maybeMIDIEvent)
import Sound.MIDI.Message.Channel (messageBody, Body(Voice))
import Sound.MIDI.Message.Channel.Voice (T(NoteOn), fromPitch, fromVelocity)

type Note = (Int, Int) -- Drum part, intensity.

open :: FilePath -> IO [(Integer, Note)]
open filename = do
    Cons t _ tracks <- fromFile filename -- Lose: division.
    let notes = mapMaybe bodyToNote $ getMessages $ mergeTracks t tracks
    return $ toPairList $ mapTime fromElapsedTime notes
    where
        -- Lose: meta events, system exclusive information, channel number.
        getMessages = mapMaybe ((>>= return . messageBody) . maybeMIDIEvent)

-- | Get note information (pitch and velocity) from a MIDI channel message.
-- Lose: all channel messages other than NoteOn (Mode information, NoteOff…).
bodyToNote :: Body -> Maybe Note
bodyToNote (Voice (NoteOn p v)) = Just (fromPitch p, fromVelocity v)
bodyToNote _                    = Nothing
