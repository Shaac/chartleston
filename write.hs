module Write (write) where

import Data.List  (partition)

import Duration (Duration(Other), duration)

-- | Write the music in Lilypond format.
write :: (Num a, Eq a, Ord b, Num b, Eq b) => [[(Duration, [(a, b)])]] -> String
write x = prefix ++ (aux' up) ++ "}\\\\{" ++ (aux' down) ++ suffix
  where
    (up, down) = unzip $ map voices x
    aux' [] = ""
    aux' (x':xs) = "            " ++ (aux x') ++ "\n" ++ (aux' xs)
    aux [] = ""
    aux ((t, [ ]):xs) = 'r' : (show t ++ " " ++ (aux xs))
    aux ((t, [n]):xs) = note' t n ++ " " ++ (aux xs)
    aux ((t, l):xs)   = '<' : (unwords $ map (note . fst) l) ++ ">" ++ (show t) ++ " " ++ (aux xs)
    note' t (n, v)
      | v < 50        = "\\parenthesize " ++ (note n) ++ (show t)
      | v == 127      = (note n) ++ (show t) ++ "->"
      | otherwise     = note n ++ (show t)

-- Separate the notes in two voices. The cymbals are up, and the rest down.
voices :: (Num a, Eq a, Eq b) =>
  [(Duration, [(a, b)])] -> ([(Duration, [(a, b)])], [(Duration, [(a, b)])])
voices x = (removeRests $ zip time xs, removeRests $ zip time ys)
  where
    (xs', ys')    = unzip $ map (partition (flip elem cymbals . fst)) notes
    (xs, ys)      = if ys' /= notes then (xs', ys') else
                    unzip $ map (partition (flip elem toms . fst)) notes
    (time, notes) = unzip x
    cymbals       = [42, 46, 49, 51, 52, 53, 55, 57, 59]
    toms          = [37, 38, 40, 41, 43, 45, 47, 48, 50]

-- Remove the rests on a voices by making previous notes longer.
removeRests :: (Eq a) => [(Duration, [a])] -> [(Duration, [a])]
removeRests = converge . (iterate (aux 0))
  where
    converge :: (Eq a) =>[[(Duration, [a])]] -> [(Duration, [a])]
    converge (x:y:xs) = if x == y then x else converge (y : xs)
    converge _        = fail "This can not occure."
    aux :: (Eq a) =>Rational ->  [(Duration, [a])] -> [(Duration, [a])]
    aux e ((t1, x):(t2, []):xs)
      | t1 + t2 /= Other && ok t1 e = (t1 + t2, x) : (aux (add (add e t1) t2) xs)
      | otherwise     = (t1, x) : (aux (add e t1) $ (t2, []) : xs)
    aux e ((t, x):xs) = (t, x) : (aux (add e t) xs)
    aux _ []          = []
    add :: Rational -> Duration -> Rational
    add e x           = let y = (duration x) + e in if y >= 1 then y - 1 else y
    ok :: Duration -> Rational -> Bool
    ok t e
      | t' == 1   = False
      | otherwise = if t' > e then True else t' <= add e t
      where t' = duration t

-- The beginning of the lilypond file.
prefix :: String
prefix = unlines [
  "\\version \"2.16.0\"",
  "",
  "#(define td15",
  "   '(",
  "     (crashcymbal    cross     #f      6)  ; Crash.",
  "     (splashcymbal   cross     #f      6)  ; TODO: crash edge.",
  "     (hihat          cross     #f      5)  ; Hi-hat.",
  "     (openhihat      cross     \"open\"  5)  ; Open hihat.",
  "     (ridecymbal     cross     #f      4)  ; Ride.",
  "     (ridecymbala    cross     #f      4)  ; TODO: ride edge.",
  "     (ridebell       triangle  #f      4)  ; TODO: ride bell.",
  "     (hightom        default   #f      3)  ; TODO: high tom rimshot.",
  "     (himidtom       default   #f      3)  ; High tom.",
  "     (lowmidtom      default   #f      2)  ; Mid tom.",
  "     (lowtom         default   #f      2)  ; TODO: mid tom rimshot.",
  "     (snare          default   #f      1)  ; Snare.",
  "     (acousticsnare  default   #f      1)  ; TODO: snare rimshot.",
  "     (sidestick      xcircle   #f      1)  ; TODO: snare cross stick.",
  "     (highfloortom   default   #f      -1) ; Floor tom.",
  "     (vibraslap      default   #f      -1) ; TODO: floor tom rimshot.",
  "     (bassdrum       default   #f      -3) ; Bass drum.",
  "     (pedalhihat     cross     #f      -5) ; Hi-hat pedal.",
  "     ))",
  "",
  "\\new DrumStaff <<",
  "    \\override Staff.TimeSignature #'style = #'() % Display 4/4 signature.",
  "    \\set Staff.beamExceptions = #'()             " ++
    "% Beam quavers two by two.",
  "    \\set DrumStaff.drumStyleTable = #(alist->hash-table td15)",
  "    \\drummode {",
  "        << {"]

-- The end of the lilypond file.
suffix :: String
suffix = unlines [
  "\\bar \"|.\"",
  "        } >>",
  "    }",
  ">>",
  "% vim:filetype=lilypond"]

-- Convert a MIDI instrument (number) to its Lilypond value.
note :: (Num a, Eq a) => a -> String
note 35 = "bda"   -- Bass drum 2
note 36 = "bd"    -- Bass Drum 1
note 37 = "ss"    -- Side Stick/Rimshot
note 38 = "sn"    -- Snare Drum 1
note 39 = "hc"    -- Hand Clap
note 40 = "sna"   -- Snare Drum 2
note 41 = "tomfl" -- Low Tom 2
note 42 = "hh"    -- Closed Hi-hat
note 43 = "tomfh" -- Low Tom 1
note 44 = "hhp"   -- Pedal Hi-hat
note 45 = "tomml" -- Mid Tom 2
note 46 = "hho"   -- Open Hi-hat
note 47 = "toml"  -- Mid Tom 1
note 48 = "tommh" -- High Tom 2
note 49 = "cymc"  -- Crash Cymbal 1
note 50 = "tomh"  -- High Tom 1
note 51 = "cymr"  -- Ride Cymbal 1
note 52 = "cymch" -- Chinese Cymbal
note 53 = "rb"    -- Ride Bell
note 54 = "tamb"  -- Tambourine
note 55 = "cyms"  -- Splash Cymbal
note 56 = "cb"    -- Cowbell
note 57 = "cymca" -- Crash Cymbal 2
note 58 = "vibs"  -- Vibra Slap
note 59 = "cymra" -- Ride Cymbal 2
note 60 = "boh"   -- High Bongo
note 61 = "bol"   -- Low Bongo
note 62 = "cghm"  -- Mute High Conga
note 63 = "cgho"  -- Open High Conga
note 64 = "cgl"   -- Low Conga
note 65 = "timh"  -- High Timbale
note 66 = "timl"  -- Low Timbale
note 67 = "agh"   -- High Agogô
note 68 = "agl"   -- Low Agogô
note 69 = "cab"   -- Cabasa
note 70 = "mar"   -- Maracas
note 71 = "whs"   -- Short Whistle
note 72 = "whl"   -- Long Whistle
note 73 = "guis"  -- Short Güiro
note 74 = "guil"  -- Long Güiro
note 75 = "cl"    -- Claves
note 76 = "wbh"   -- High Wood Block
note 77 = "wbl"   -- Low Wood Block
note 78 = "cuim"  -- Mute Cuíca
note 79 = "cuio"  -- Open Cuíca
note 80 = "trim"  -- Mute Triangle
note 81 = "trio"  -- Open Triangle
note _  = fail "Unexisting note."
