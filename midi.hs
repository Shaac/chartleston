module Midi (open, Note) where

import Data.EventList.Relative.TimeBody (mapMaybe, mapTime, toPairList)
import Control.Monad                    (liftM)

import Sound.MIDI.File                  (fromElapsedTime, mergeTracks, T(Cons))
import Sound.MIDI.File.Load             (fromFile)
import Sound.MIDI.File.Event            (maybeMIDIEvent)
import Sound.MIDI.Message.Channel       (Body(Voice), messageBody)
import Sound.MIDI.Message.Channel.Voice (fromPitch, fromVelocity, T(NoteOn))

type Note = (Int, Int) -- Drum part, intensity.

-- | Open a MIDI file, parse it, and return the notes list.
open :: FilePath -> IO [(Integer, Note)]
open filename = do
  -- Read file.
  Cons t _ tracks <- fromFile filename -- Lose: division.
  -- Parse the MIDI files to get the notes.
  let notes = mapMaybe bodyToNote $ getMessages $ mergeTracks t tracks
  -- Retun the pairs time, note.
  return $ toPairList $ mapTime fromElapsedTime notes
  where
    -- Lose: meta events, system exclusive information, channel number.
    getMessages = mapMaybe (liftM messageBody . maybeMIDIEvent)

-- | Get note information (pitch and velocity) from a MIDI channel message.
-- Lose: all channel messages other than NoteOn (Mode information, NoteOff…).
bodyToNote :: Body -> Maybe Note
bodyToNote (Voice (NoteOn p v)) = Just (fromPitch p, fromVelocity v)
bodyToNote _                    = Nothing
