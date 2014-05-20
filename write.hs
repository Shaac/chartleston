module Write (write) where

import Duration (Duration, getNote)
import Note     (Note, show', isFlam)

-- | Write the music in Lilypond format.
write :: [([(Duration, [Note])], [(Duration, [Note])])] -> String
write x = prefix ++ (concatMap write' x) ++ suffix
  where
    write' x'= "        << {\n            " ++ (aux up) ++ "\n        } \\\\ {\n            " ++ (aux down) ++ "\n        } >>\n"
      where (up, down) = (fst x', snd x')
    aux [] = ""
    aux ((t, [ ]):xs) = getNote t ("r" ++) ++ " " ++ (aux xs)
    aux ((t, [n]):xs) = getNote t (show' n) ++ " " ++ (aux xs)
    aux ((t, l@(n:_)):xs)
--      | isFlam l  = getNote t ("\\acciaccatura {\\once\\stemUp " ++ (show' n) ++ "8}" ++ (show n)) ++ (aux xs)
      | otherwise = getNote t (('<' : (unwords $ map show l) ++ ">") ++) ++ " " ++ (aux xs)

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
  "% vim:filetype=lilypond"]
