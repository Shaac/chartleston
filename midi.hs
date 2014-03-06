module Midi where

import Sound.MIDI.File.Load (fromFile)
import Sound.MIDI.File

data Music = Music {division :: Division, track :: Track}
    deriving (Show)

open :: FilePath -> IO Music
open filename = do
    Cons t d tracks <- fromFile filename
    return $ Music d $ mergeTracks t tracks
