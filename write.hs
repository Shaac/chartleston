module Write where

import Data.List  (partition)
import Data.Ratio (denominator)

write :: (Num a, Eq a, Eq b) => [(Integer, [(a, b)])] -> String
write x = prefix ++ (aux up) ++ "}\\\\{" ++ (aux down) ++ suffix
  where
    (up, down) = voices x
    aux [] = ""
    aux ((t, []):xs) = 'r' : (end t xs)
    aux ((t, [(n, _)]):xs) = note n ++ (end t xs)
    aux ((t, l):xs) = '<' : (unwords $ map (note . fst) l) ++ ">" ++ (end t xs)
    end t xs = (show t) ++ " " ++ (aux xs)

voices :: (Num a, Eq a, Eq b) =>
  [(Integer, [(a, b)])] -> ([(Integer, [(a, b)])], [(Integer, [(a, b)])])
voices x = (removeRests $ zip time xs, removeRests $ zip time ys)
  where
    (xs, ys)      = unzip $ map (partition (flip elem cymbals . fst)) notes
    (time, notes) = unzip x
    cymbals       = [42, 46, 49, 51, 52, 53, 55, 57, 59]

removeRests :: (Num a, Eq a, Eq b) =>
    [(Integer, [(a, b)])] -> [(Integer, [(a, b)])]
removeRests = convergence . (iterate (aux 0))
  where
    convergence (x:y:xs) = if x == y then x else convergence (y : xs)
    convergence _        = fail "This can not occure."
    aux e ((t1, x):(t2, []):xs)
      | t1 == t2 && t1 <= denominator (add e t1) =
                           (round $ (fromInteger t1 / 2 :: Double), x) :
                           (aux (add e (t1 * 2))xs)
      | otherwise        = (t1, x) : (aux (add e t1) $ (t2, []) : xs)
    aux e ((t, x):xs)    = (t, x) : (aux (add e t) xs)
    aux _ []             = []
    add e x              = let y = e + ((1 / (fromInteger x)) :: Rational) in
                           if y >= 1 then y - 1 else y

prefix :: String
prefix = unlines [
  "\\version \"2.16.0\"",
  "",
  "#(define standard '(",
  "    (splashcymbal   cross    #f  6)",
  "    (hihat          cross    #f  5)",
  "    (hightom        default  #f  4)",
  "    (himidtom       default  #f  3)",
  "    (lowmidtom      default  #f  2)",
  "    (snare          default  #f  1)",
  "    (lowtom         default  #f  0)",
  "    (highfloortom   default  #f  -1)",
  "    (lowfloortom    default  #f  -2)",
  "    (bassdrum       default  #f  -3)))",
  "",
  "\\new DrumStaff <<",
  "    \\override Staff.TimeSignature #'style = #'() % Display 4/4 signature.",
  "    \\set Staff.beamExceptions = #'() % Beam quavers two by two.",
  "    \\set DrumStaff.drumStyleTable = #(alist->hash-table standard)",
  "    \\drummode {",
  "        << {"]

suffix :: String
suffix = "\\bar \"|.\"\n}>>}>>\n"

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
