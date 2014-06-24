module Note (Note, show', fromPair, isCymbal, isTom, flams, pedals) where

import Control.Arrow (first)


---------------
-- Constants --
---------------

-- Threshold value for a note to be considered as ghost.
threshold :: Int
threshold = 50


----------------
-- Structures --
----------------

data Note = Note (Instrument, Int) | Flam (Instrument)

data Velocity = Ghost | Regular | Accent deriving (Eq, Ord, Show)

instance Eq Note where
  x@(Note (p, _)) == x'@(Note (p', _)) = p == p' && velocity x == velocity x'
  Flam (p)        == Flam (p')         = p == p'
  _               == _                 = False

instance Show Note where
  show = flip show' ""


------------------------
-- Exported functions --
------------------------

-- | Take a note and the lilypond representation of the duration.
-- Return the lilypond representation of the note.
show' :: Note -> String -> String
show' x@(Note (i, _)) = let instr = show i in case velocity x of
  Ghost   -> (("\\parenthesize " ++ instr) ++)
  Regular -> (instr ++)
  Accent  -> (instr ++) . (++ "->")
show' (Flam i) = (("\\acciaccatura{\\once\\stemUp " ++ i' ++ "8}" ++ i') ++)
  where i' = show i

-- | Create a note from a pair (pitch, velocity).
fromPair :: (Int, Int) -> Note
fromPair = Note . first td15

-- | Determine if a note is a cymbal.
isCymbal :: Note -> Bool
isCymbal = flip elem [CrashCymbal, HalfOpenHiHat, HiHat, OpenHiHat, RideBell,
                      RideCymbal] . instrument

-- | Determine if a note is a tom.
isTom :: Note -> Bool
isTom = flip elem [CrossStick, FloorTom, HighTom, MidTom, Rimshot, Snare]
        . instrument

-- | Detect the flams in a list of simultaneous notes.
flams :: [Note] -> [Note]
flams (x@(Note (i, _)) : xs)
  | i `elem` (map instrument xs) = Flam i :
                                   flams (filter ((/= i) . instrument) xs)
  | otherwise                    = x : flams xs
flams (x : xs)                   = x : flams xs
flams x                          = x

pedals :: [Note] -> [Note]
pedals xs
  | HiHat `elem` map instrument xs = filter ((/= PedalHiHat) . instrument) xs
  | otherwise                      = xs

---------------------
-- Local functions --
---------------------

-- Get the instrument of a note.
instrument :: Note -> Instrument
instrument (Note (i, _)) = i
instrument (Flam (i))    = i

-- Get the velocity (i.e. the strength) of a note.
velocity :: Note -> Velocity
velocity (Note (_, v)) | v == 127      = Accent
velocity (Note (_, v)) | v > threshold = Regular
velocity (Note (_, _))                 = Ghost
velocity _                             = Regular


data Instrument = BassDrum
                | CrashCymbal
                | CrossStick
                | FloorTom
                | HalfOpenHiHat
                | HiHat
                | HighTom
                | MidTom
                | OpenHiHat
                | PedalHiHat
                | RideBell
                | RideCymbal
                | Rimshot
                | Snare
                | Other deriving Eq

instance Show Instrument where
  show BassDrum      = "bd"
  show CrashCymbal   = "cymc"
  show CrossStick    = "sn" -- TODO
  show FloorTom      = "tomfh"
  show HalfOpenHiHat = "hhho"
  show HiHat         = "hh"
  show HighTom       = "tomh"
  show MidTom        = "toml"
  show OpenHiHat     = "hho"
  show PedalHiHat    = "hhp"
  show RideBell      = "rb"
  show RideCymbal    = "cymr"
  show Rimshot       = "sn" -- TODO
  show Snare         = "sn"
  show Other         = "ss" -- Dummy

-- Convert a MIDI instrument (number) to its Lilypond value.
td15 :: (Num a, Eq a) => a -> Instrument
td15 36 = BassDrum      -- Bass Drum 1
td15 37 = CrossStick    -- Side Stick/Rimshot
td15 38 = Snare         -- Snare Drum 1
td15 40 = Rimshot       -- Snare Drum 2
td15 42 = HiHat         -- Closed Hi-hat
td15 43 = FloorTom      -- Low Tom 1
td15 44 = PedalHiHat    -- Pedal Hi-hat
td15 45 = MidTom        -- Mid Tom 2
td15 46 = HalfOpenHiHat -- Open Hi-hat
td15 47 = MidTom        -- Mid Tom 1 (TODO: this is rimshot)
td15 48 = HighTom       -- High Tom 2
td15 49 = CrashCymbal   -- Crash Cymbal 1
td15 50 = HighTom       -- High Tom 1 (TODO: this is rimshot)
td15 51 = RideCymbal    -- Ride Cymbal 1
td15 53 = RideBell      -- Ride Bell
td15 55 = CrashCymbal   -- Splash Cymbal (TODO: this is the edge)
td15 58 = FloorTom      -- Vibra Slap (TODO: this is floor tom rimshot)
td15 59 = RideCymbal    -- Ride Cymbal 2 (TODO: this is the edge)
td15 26 = OpenHiHat     -- TD15 open hi-hat
td15 _  = Other
