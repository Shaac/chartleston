\version "2.16.0"

#(define td15
   '(
     (crashcymbal    cross     #f          6)  ; Crash.
     (hihat          cross     #f          5)  ; Hi-hat.
     (openhihat      cross     "open"      5)  ; Open hihat.
     (halfopenhihat  cross     "halfopen"  5)  ; Half open hihat.
     (ridecymbal     cross     #f          4)  ; Ride.
     (ridebell       triangle  #f          4)  ; TODO: ride bell.
     (hightom        default   #f          3)  ; High tom.
     (lowtom         default   #f          2)  ; Mid tom.
     (snare          default   #f          1)  ; Snare.
     (highfloortom   default   #f          -1) ; Floor tom.
     (bassdrum       default   #f          -3) ; Bass drum.
     (pedalhihat     cross     #f          -5) ; Hi-hat pedal.
     ))

\header {
    title = "Smells Like Teen Spirit"
}

\new DrumStaff <<
    \override Staff.TimeSignature #'style = #'() % Display 4/4 signature.
    \set Staff.beamExceptions = #'()             % Beam quavers two by two.
    \set DrumStaff.drumStyleTable = #(alist->hash-table td15)
    \drummode {
        \compressFullBarRests
        \override MultiMeasureRest #'expand-limit = #1
        \set countPercentRepeats = ##t
        \set repeatCountVisibility = #(every-nth-repeat-count-visible 4)
        R1 * 3
        << {
            r8 hh8 r8 hh8 r8 hh8 r4
        } \\ {
            \acciaccatura{\once\stemUp sn8}sn16 bd16 r16 bd16
            \acciaccatura{\once\stemUp sn8}sn16 bd16 r16 bd16
            \acciaccatura{\once\stemUp sn8}sn16 bd16 r16 bd16
            \acciaccatura{\once\stemUp sn8}sn8 bd8
        } >>
        \repeat percent 7 {
        << {
            cymc4 hh4 hh4 hh4
        } \\ {
            bd8. bd16 sn8. sn16 bd16 sn16 bd16 bd16 sn8 bd8
        } >>
        }
        << {
            cymc4 hh4 hh4 r4
        } \\ {
            bd8. bd16 sn8 bd8 bd8 sn16 sn16 sn16 sn16 sn16 sn16
        } >>
        \mark \markup { \musicglyph#"scripts.segno" }
        \repeat volta 6
        << {
            hh8 hh8 hh8 hh8 hh8 hh8 hh8 hh8
        } \\ {
            bd8 bd8 sn4 bd8 bd8 sn4
        } >>
        \alternative {
            {        << {
            hh8 hh8 hh8 hh8 hh8 hh8 hh8 hhho8
        } \\ {
            bd8 bd8 sn4 bd8 bd8 sn4
        } >>
            }
            {        << {
            hh8 hh8 hh8 hh8 hh8 hh8 cymc4
        } \\ {
            bd8 bd8 sn4 bd8 bd8 sn4
        } >>
            }
        }
        \repeat percent 2 {
        << {
            cymc8 cymr8 cymr8 cymr8 cymr8 cymr8 cymr8 cymr8
        } \\ {
            bd8 bd8 sn4 bd8 bd8 sn4
        } >>
        << {
            cymr8 cymr8 cymr8 cymr8 cymr8 cymr8 cymr8 cymr8
        } \\ {
            bd8 bd8 sn4 bd8 bd8 sn4
        } >>
        }
        \repeat volta 2
        << {
            cymc8 cymr8 cymr8 cymr8 cymr8 cymr8 cymr8 cymr8
        } \\ {
            bd8 bd8 sn8 bd8 bd8 bd8 sn8 bd8
        } >>
        \alternative {
            {        << {
            cymr8 cymr8 cymr8 cymr8 cymr8 cymr8 cymr8 cymr8
        } \\ {
            bd8 bd8 sn8 bd8 bd8 bd8 sn8 bd8
        } >>
            }
            {        sn16 sn16 sn16 sn16 sn16 sn16 sn16 sn16 sn16 sn16 sn16 bd16
        \acciaccatura{\once\stemUp sn8}sn8 bd8
            }
        }
        \repeat percent 8 {
        << {
            cymc4 hh4 hh4 hh4
        } \\ {
            bd8. bd16 sn8. sn16 bd16 sn16 bd16 bd16 sn8 bd8
        } >>
        }
        \repeat percent 3 {
        << {
            cymc4 cymc4 cymc4 cymc4
        } \\ {
            bd8. bd16 sn8. sn16 bd16 sn16 bd16 bd16 sn8 bd8
        } >>
        }
        << {
            cymc4 cymc4 cymc4 cymc4
        } \\ {
            bd8. bd16 sn8 bd16 bd16 sn8 bd16 bd16 sn8 bd8
        } >>
        << {
            cymc4 cymc4 cymc2
        } \\ {
            bd8 bd8 sn8. bd16 sn8 bd4.
        } >>
        << {
            cymc4 hh4 r8 hh8 r8 hh8
        } \\ {
            bd8 bd8 sn8. bd16 \acciaccatura{\once\stemUp sn8}sn16 bd16 r16 bd16
            \acciaccatura{\once\stemUp sn8}sn16 bd16 r16 bd16
        } >>
        << {
            cymc4 hh4 hh2
        } \\ {
            bd8 bd8 sn8. bd16 sn8 bd4.
        } >>
        << {
            cymc4 hh4 hh4 r4
        } \\ {
            bd8 bd8 sn8 bd8 bd8 sn16 sn16 sn16 sn16 sn16 sn16
        } >>
        \mark \markup { \musicglyph#"scripts.segno" }
        \repeat percent 2 {
        << {
            cymc4 hh4 hh4 hh4
        } \\ {
            bd8. bd16 sn8. sn16 bd16 sn16 bd16 bd16 sn8 bd8
        } >>
        }
        << {
            cymc4 hh4 hh4 hh4
        } \\ {
            bd8. bd16 sn8. sn16 bd16 <sn bd >16 bd8 sn8 bd8
        } >>
        \repeat percent 2 {
        << {
            cymc4 hh4 hh4 hh4
        } \\ {
            bd8. bd16 sn8. sn16 bd16 sn16 bd16 bd16 sn8 bd8
        } >>
        }
        \repeat percent 2 {
        << {
            cymc4 hh4 hh4 hh4
        } \\ {
            bd8. bd16 sn8. sn16 bd16 <sn bd >8 bd16 sn8 bd8
        } >>
        }
        \repeat percent 2 {
        << {
            cymc4 hh4 hh4 hh4
        } \\ {
            bd8. bd16 sn8. sn16 bd16 sn16 bd16 bd16 sn8 bd8
        } >>
        << {
            cymc4 hh4 hh4 hh4
        } \\ {
            bd8. bd16 sn8. sn16 bd8 <sn bd >16 bd16 sn8 bd8
        } >>
        }
        \repeat percent 2 {
        << {
            cymc4 hh4 hh4 hh4
        } \\ {
            bd8. bd16 sn8. sn16 bd16 sn16 bd16 bd16 sn8 bd8
        } >>
        }
        << {
            cymc4 hh4 hh4 hh4
        } \\ {
            bd8. bd16 sn8. sn16 bd16 <sn bd >8
            \acciaccatura{\once\stemUp bd8}bd16 sn8 bd8
        } >>
        << {
            cymc4 hh4 hh4 hh4
        } \\ {
            bd8. bd16 sn8. sn16 bd16 sn16 bd16 bd16 sn8 bd8
        } >>
        << {
            cymc4 hh4 hh4 r4
        } \\ {
            bd8. bd16 sn8. sn16 bd16 sn16 sn16 sn16 sn16 sn16 sn16 sn16
        } >>
        << {
            hh8 hh8 hh8 hh8 hh8 hh8 hh8 hh8
        } \\ {
            bd8 bd8 sn4 bd8 bd8 sn4
        } >>
        << {
            hh8 hh8 hh8 hh8 hh8 hh8 hh8 hho8
        } \\ {
            bd8 bd8 sn4 bd8 bd8 sn4
        } >>
        \repeat volta 5
        << {
            hh8 hh8 hh8 hh8 hh8 hh8 hh8 hh8
        } \\ {
            bd8 bd8 sn4 bd8 bd8 sn4
        } >>
        \alternative {
            {        << {
            hh8 hh8 hh8 hh8 hh8 hh8 hh8 hhho8
        } \\ {
            bd8 bd8 sn4 bd8 bd8 sn4
        } >>
            }
            {        << {
            hh8 hh8 hh8 hh8 hh8 hh8 cymc4
        } \\ {
            bd8 bd8 sn4 bd8 bd8 sn4
        } >>
            }
        }
        \repeat percent 2 {
        << {
            cymc8 cymr8 cymr8 cymr8 cymr8 cymr8 cymr8 cymr8
        } \\ {
            bd8 bd8 sn4 bd8 bd8 sn4
        } >>
        << {
            cymr8 cymr8 cymr8 cymr8 cymr8 cymr8 cymr8 cymr8
        } \\ {
            bd8 bd8 sn4 bd8 bd8 sn4
        } >>
        }
        << {
            cymc8 cymr8 cymr8 cymr8 cymr8 cymr8 cymr8 cymr8
        } \\ {
            bd8 bd8 sn8 bd8 bd8 bd8 sn8 bd8
        } >>
        << {
            cymr8 cymr8 cymr8 cymr8 cymr8 cymr8 cymr8 cymr8
        } \\ {
            bd8 bd8 sn8 bd8 bd8 bd8 sn8 bd8
        } >>
        << {
            cymc8 cymr8 cymr8 cymr8 cymr8 cymr8 cymr8 cymr8
        } \\ {
            bd8 bd8 sn8 bd8 bd8 bd8 sn8 bd8
        } >>
        sn16 sn16 sn16 sn16 sn16 sn16 sn16 sn16 sn16 sn16 bd8
        \acciaccatura{\once\stemUp sn8}sn8 bd8
        << {
            cymc4 hh4 hh4 hh4
        } \\ {
            <bd tomh >8. bd16 sn8. sn16 bd16 sn16 bd16 bd16 sn8 bd8
        } >>
        \repeat percent 11 {
        << {
            cymc4 hh4 hh4 hh4
        } \\ {
            bd8. bd16 sn8. sn16 bd16 sn16 bd16 bd16 sn8 bd8
        } >>
        }
        << {
            cymc4 hh4 cymc4 cymc4
        } \\ {
            bd8. bd16 sn8. sn16 bd16 sn16 bd16 bd16 sn8 bd8
        } >>
        << {
            cymc4 cymc4 cymc4 cymc4
        } \\ {
            bd8. bd16 sn8. sn16 bd16 <sn bd >8 bd16 sn8 bd8
        } >>
        \repeat percent 5 {
        << {
            cymc4 cymc4 cymc4 cymc4
        } \\ {
            bd8. bd16 sn8. bd16 sn16 bd8 bd16 sn8 bd8
        } >>
        }
        << {
            cymc4 cymc4 cymc4 r4
        } \\ {
            bd8. bd16 sn8 bd8 sn16 bd8 bd16 \acciaccatura{\once\stemUp sn8}sn8
            bd8
        } >>
        << {
            cymc1
        } \\ {
            bd1
        } >>

        \bar "|."
    }
>>

% vim:filetype=lilypond
