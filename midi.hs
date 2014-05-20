module Midi (open) where

import Data.EventList.Relative.TimeBody (mapMaybe, mapTime, toPairList)
import Control.Monad                    (liftM, mfilter)
import Numeric.NonNegative.Wrapper      (toNumber)

import Sound.MIDI.File                  (explicitNoteOff, mergeTracks,
                                         secondsFromTicks, T(Cons))
import Sound.MIDI.File.Load             (fromFile)
import Sound.MIDI.File.Event            (maybeMIDIEvent)
import Sound.MIDI.Message.Channel       (Body(Voice), fromChannel,
                                         messageBody, messageChannel)
import Sound.MIDI.Message.Channel.Voice (fromPitch, fromVelocity, T(NoteOn))

import Note (Note, fromPair)

-- | Open a MIDI file, parse it, and return the notes list.
open :: FilePath -> IO [(Rational, Note)]
open filename = do
  -- Read file.
  Cons typ division tracks <- liftM explicitNoteOff $ fromFile filename
  let track = secondsFromTicks division $ mergeTracks typ tracks
  -- Parse the MIDI files to get the notes.
  let notes = mapMaybe bodyToNote $ getMessages track
  -- Return the pairs (time in seconds, note).
  return $ toPairList $ mapTime toNumber notes
  where
    -- Lose: meta events, system exclusive information.
    getMessages  = mapMaybe $ liftM messageBody . isPercussion . maybeMIDIEvent
    isPercussion = mfilter $ (== 9) . fromChannel . messageChannel

-- Get note information (pitch and velocity) from a MIDI channel message.
-- Lose: all channel messages other than NoteOn (Mode information, NoteOff…).
bodyToNote :: Body -> Maybe Note
bodyToNote (Voice (NoteOn p v)) = Just $ fromPair (fromPitch p, fromVelocity v)
bodyToNote _                    = Nothing
