module Note (Note, show', fromPair, isCymbal, isTom, isFlam) where

data Note = N (Int, Int)
data Velocity = Ghost | Regular | Accent deriving (Eq, Ord, Show)

instance Eq Note where
  a == b = pitch a == pitch b && velocity a == velocity b

threshold :: Int
threshold = 50

show' :: Note -> String -> String
show' x = let instr = instrument (pitch x) in case velocity x of
  Ghost   -> (("\\parenthesize " ++ instr) ++)
  Regular -> (instr ++)
  Accent  -> (instr ++) . (++ "->")

instance Show Note where
  show = flip show' ""

fromPair :: (Int, Int) -> Note
fromPair = N

isCymbal :: Note -> Bool
isCymbal = flip elem [42, 46, 49, 51, 52, 53, 55, 57, 59] . pitch

isTom :: Note -> Bool
isTom = flip elem [37, 38, 40, 41, 43, 45, 47, 48, 50] . pitch

isFlam :: [Note] -> Bool
isFlam [N(x, _), N(y, _)] = x == y
isFlam _                  = False


pitch :: Note -> Int
pitch (N (p, _)) = p

velocity :: Note -> Velocity
velocity (N (_, v)) | v == 127      = Accent
velocity (N (_, v)) | v > threshold = Regular
velocity _                          = Ghost

-- Convert a MIDI instrument (number) to its Lilypond value.
instrument :: (Num a, Eq a) => a -> String
instrument 35 = "bda"   -- Bass drum 2
instrument 36 = "bd"    -- Bass Drum 1
instrument 37 = "ss"    -- Side Stick/Rimshot
instrument 38 = "sn"    -- Snare Drum 1
instrument 39 = "hc"    -- Hand Clap
instrument 40 = "sna"   -- Snare Drum 2
instrument 41 = "tomfl" -- Low Tom 2
instrument 42 = "hh"    -- Closed Hi-hat
instrument 43 = "tomfh" -- Low Tom 1
instrument 44 = "hhp"   -- Pedal Hi-hat
instrument 45 = "tomml" -- Mid Tom 2
instrument 46 = "hho"   -- Open Hi-hat
instrument 47 = "toml"  -- Mid Tom 1
instrument 48 = "tommh" -- High Tom 2
instrument 49 = "cymc"  -- Crash Cymbal 1
instrument 50 = "tomh"  -- High Tom 1
instrument 51 = "cymr"  -- Ride Cymbal 1
instrument 52 = "cymch" -- Chinese Cymbal
instrument 53 = "rb"    -- Ride Bell
instrument 54 = "tamb"  -- Tambourine
instrument 55 = "cyms"  -- Splash Cymbal
instrument 56 = "cb"    -- Cowbell
instrument 57 = "cymca" -- Crash Cymbal 2
instrument 58 = "vibs"  -- Vibra Slap
instrument 59 = "cymra" -- Ride Cymbal 2
instrument 60 = "boh"   -- High Bongo
instrument 61 = "bol"   -- Low Bongo
instrument 62 = "cghm"  -- Mute High Conga
instrument 63 = "cgho"  -- Open High Conga
instrument 64 = "cgl"   -- Low Conga
instrument 65 = "timh"  -- High Timbale
instrument 66 = "timl"  -- Low Timbale
instrument 67 = "agh"   -- High Agogô
instrument 68 = "agl"   -- Low Agogô
instrument 69 = "cab"   -- Cabasa
instrument 70 = "mar"   -- Maracas
instrument 71 = "whs"   -- Short Whistle
instrument 72 = "whl"   -- Long Whistle
instrument 73 = "guis"  -- Short Güiro
instrument 74 = "guil"  -- Long Güiro
instrument 75 = "cl"    -- Claves
instrument 76 = "wbh"   -- High Wood Block
instrument 77 = "wbl"   -- Low Wood Block
instrument 78 = "cuim"  -- Mute Cuíca
instrument 79 = "cuio"  -- Open Cuíca
instrument 80 = "trim"  -- Mute Triangle
instrument 81 = "trio"  -- Open Triangle
instrument _  = "tt" -- Phony instrument, so that in case of error it can compile
