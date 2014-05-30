module Note (Note, show', fromPair, isCymbal, isTom, flams) where

data Note = Note (Int, Int) | Flam (Int)
data Velocity = Ghost | Regular | Accent deriving (Eq, Ord, Show)

instance Eq Note where
  x@(Note (p, _)) == x'@(Note (p', _)) = p == p' && velocity x == velocity x'
  Flam (p) == Flam (p')                = p == p'
  _ == _                               = False

threshold :: Int
threshold = 50

show' :: Note -> String -> String
show' x@(Note (p, _)) = let instr = instrument p in case velocity x of
  Ghost   -> (("\\parenthesize " ++ instr) ++)
  Regular -> (instr ++)
  Accent  -> (instr ++) . (++ "->")
show' (Flam p) = (("\\acciaccatura{\\once\\stemUp " ++ i ++ "8}" ++ i) ++)
  where i = instrument p

instance Show Note where
  show = flip show' ""

fromPair :: (Int, Int) -> Note
fromPair = Note

isCymbal :: Note -> Bool
isCymbal = flip elem [26, 42, 46, 49, 51, 52, 53, 55, 57, 59] . pitch

isTom :: Note -> Bool
isTom = flip elem [37, 38, 40, 41, 43, 45, 47, 48, 50] . pitch

flams :: [Note] -> [Note]
flams (x@(Note (p, _)) : xs)
  | p `elem` (map pitch xs) = Flam p : flams (filter ((/= p) . pitch) xs)
  | otherwise               = x : flams xs
flams (x : xs)              = x : flams xs
flams x                     = x

pitch :: Note -> Int
pitch (Note (p, _)) = p
pitch (Flam (p))    = p

velocity :: Note -> Velocity
velocity (Note (_, v)) | v == 127      = Accent
velocity (Note (_, v)) | v > threshold = Regular
velocity (Note (_, _))                 = Ghost
velocity _                             = Regular

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
instrument 46 = "hhho"  -- Open Hi-hat; TD15 half open
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
instrument 26 = "hho"   -- TD15 open hi-hat
instrument _  = "tt" -- Phony; so that in case of error it can compile
