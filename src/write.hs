module Write (write) where

import Duration (Duration, showNote)
import Note     (Note, show')
import Score    (Score, title, score)


------------------------
-- Exported functions --
------------------------

-- | Write the music in Lilypond format.
write :: Score [([(Duration, [Note])], [(Duration, [Note])])] -> String
write s = text $ score s
  where
    text = (prefix (title s) ++) . (++ suffix) . (concatMap $ uncurry voices)


---------------------
-- Local functions --
---------------------

-- Get the Lilypond notation for an entire measure.
voices :: [(Duration, [Note])] -> [(Duration, [Note])] -> String
voices up down
  | all (null . snd) up   = voice 2 down
  | all (null . snd) down = voice 2 up
  | otherwise             = "        << {\n" ++ (voice 3 up) ++ "        } \\"
                            ++ "\\ {\n" ++ (voice 3 down) ++ "        } >>\n"

-- Get the Lilypond notation for an entire voice. Limit the line length.
voice :: Int -> [(Duration, [Note])] -> String
voice t = pretty . (map $ uncurry notes)
  where
    pretty = concatMap (((replicate (t * 4) ' ') ++) . (++ "\n")) . (lines' "")
    lines' acc []                 = [acc]
    lines' ""  (x : xs)           = lines' x xs
    lines' acc l@(x : xs)
      | length acc + length x > m = acc : lines' "" l
      | otherwise                 = lines' (acc ++ " " ++ x) xs
    m = 78 - t * 4

-- Get the Lilypond notation for simultaneous notes.
notes :: Duration -> [Note] -> String
notes d [ ] = showNote d ("r" ++)
notes d [n] = showNote d (show' n)
notes d  l  = showNote d (('<' : (unwords $ map show l) ++ " >") ++)


---------------------
-- Local variables --
---------------------

-- The beginning of the lilypond file.
prefix :: String -> String
prefix title' = unlines [
  "\\version \"2.16.0\"",
  "",
  "#(define td15",
  "   '(",
  "     (crashcymbal    cross     #f          6)  ; Crash.",
  "     (splashcymbal   cross     #f          6)  ; TODO: crash edge.",
  "     (hihat          cross     #f          5)  ; Hi-hat.",
  "     (openhihat      cross     \"open\"      5)  ; Open hihat.",
  "     (halfopenhihat  cross     \"halfopen\"  5)  ; Half open hihat.",
  "     (ridecymbal     cross     #f          4)  ; Ride.",
  "     (ridecymbala    cross     #f          4)  ; TODO: ride edge.",
  "     (ridebell       triangle  #f          4)  ; TODO: ride bell.",
  "     (hightom        default   #f          3)  ; TODO: high tom rimshot.",
  "     (himidtom       default   #f          3)  ; High tom.",
  "     (lowmidtom      default   #f          2)  ; Mid tom.",
  "     (lowtom         default   #f          2)  ; TODO: mid tom rimshot.",
  "     (snare          default   #f          1)  ; Snare.",
  "     (acousticsnare  default   #f          1)  ; TODO: snare rimshot.",
  "     (sidestick      xcircle   #f          1)  ; TODO: snare cross stick.",
  "     (highfloortom   default   #f          -1) ; Floor tom.",
  "     (vibraslap      default   #f          -1) ; TODO: floor tom rimshot.",
  "     (bassdrum       default   #f          -3) ; Bass drum.",
  "     (pedalhihat     cross     #f          -5) ; Hi-hat pedal.",
  "     ))",
  "",
  "\\header {",
  "    title = \"" ++ title' ++ "\"",
  "}",
  "",
  "\\new DrumStaff <<",
  "    \\override Staff.TimeSignature #'style = #'() % Display 4/4 signature.",
  "    \\set Staff.beamExceptions = #'()             " ++
    "% Beam quavers two by two.",
  "    \\set DrumStaff.drumStyleTable = #(alist->hash-table td15)",
  "    \\drummode {",
  "        \\compressFullBarRests",
  "        \\override MultiMeasureRest #'expand-limit = #1"]

-- The end of the lilypond file.
suffix :: String
suffix = unlines [
  "",
  "        \\bar \"|.\"",
  "    }",
  ">>",
  "",
  "% vi" ++ "m:filetype=lilypond"]
