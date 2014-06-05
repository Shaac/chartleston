module Duration (Duration, fromFractional, guess, isNote, showNote,
PossibleDuration(..)) where

import Data.Ratio (numerator)

---------------
-- Structure --
---------------

-- Basic: The note length is 1 / 2^n of that of the measure.
-- Dotted: The note lengt is 1.5 times that of a basic note.
data Duration = Basic Integer | Dotted Integer | Other Rational deriving Eq

data PossibleDuration = PossibleDuration {
    value    :: Duration,
    original :: Rational,
    err      :: Rational
}

instance Num Duration where
  -- | Two notes can be added if the sum is a regular note.
  a@(Basic x) + b@(Basic y)
    | x == y     = Basic (x - 1)
    | x == y + 1 = Dotted y
    | x + 1 == y = Dotted x
    | otherwise  = Other (duration a + duration b)
  a@(Basic x) + b@(Dotted y)
    | x == y + 1 = Basic (y - 1)
    | otherwise = Other (duration a + duration b)
  a@(Dotted x) + b@(Dotted y)
    | x == y = Dotted (x - 1)
    | otherwise = Other (duration a + duration b)
  (Other r) + x = fromFractional' (r + duration x :: Rational)
  a + b = b + a

  -- | Soustraction of two notes will probably never be used but still.
  a@(Basic x) - b@(Basic y)
    | x + 1 == y = Basic y
    | x + 2 == y = Dotted (y + 1)
    | otherwise  = Other (duration a - duration b)
  a@(Dotted x) - b@(Basic y)
    | x + 1 == y = Basic x
    | otherwise  = Other (duration a - duration b)
  a@(Basic x) - b@(Dotted y)
    | x + 1 == y = Basic (y - 1)
    | otherwise  = Other (duration a - duration b)
  a@(Dotted x) - b@(Dotted y)
    | x + 1 == y = Dotted y
    | otherwise  = Other (duration a - duration b)
  a - b          = fromFractional' (duration a - duration b :: Rational)

  (Basic  x) * (Basic  y) = Basic  $ x + y
  (Basic  x) * (Dotted y) = Dotted $ x + y
  (Dotted x) * (Dotted y) = Dotted $ x + y - 1
  (Other  x) * d          = fromFractional' $ x + duration d
  a          * b          = b * a

  signum (Other x)
    | x == 0       = Other 0
    | x >  0       = Basic 0
    | x <  0       = Other $ - 1
  signum _         = Basic 0

  abs (Other x) = Other $ abs x
  abs d         = d

  fromInteger = fromFractional' . (fromInteger :: Integer -> Rational)

instance Enum Duration where
  toEnum i
    | i < 7 && i `mod` 2 == 0 = Basic  $  toInteger i      `div` 2
    | i < 7                   = Dotted $ (toInteger i + 1) `div` 2
    | otherwise               = Basic $ toInteger $ i - 3

  fromEnum (Basic  x)
    | x < 4     = fromInteger $ x * 2
    | otherwise = fromInteger $ x + 3
  fromEnum (Dotted x)
    | x < 4     = fromInteger $ x * 2 - 1
    | otherwise = error "Such small dotted can not be an Enum."
  fromEnum _          = error "Other can not be an Enum."

instance Ord Duration where
  a <= b = (duration a :: Rational) <= (duration b :: Rational)

instance Fractional Duration where
  recip   (Basic  x) = Basic $ - x
  recip n@(Dotted _) = Other $ recip $ duration n
  recip   (Other  x) = fromFractional' $ recip x

  fromRational = fromFractional'

instance Real Duration where
  toRational = duration

instance RealFrac Duration where
  properFraction = properFraction . duration

instance Show Duration where
  -- | Display the duration name in UK English.
  show (Basic (-3)) = "maxima"
  show (Basic (-2)) = "longa"
  show (Basic (-1)) = "breve"
  show (Basic 0)    = "semibreve"
  show (Basic 1)    = "minim"
  show (Basic 2)    = "crotchet"
  show (Basic 3)    = "quaver"
  show (Basic 4)    = "semiquaver"
  show (Basic 5)    = "demisemiquaver"
  show (Basic 6)    = "hemidemisemiquaver"
  show (Basic 7)    = "semihemidemisemiquaver"
  show (Basic x)    = "1 / (2^" ++ (show x) ++ ")"
  show (Dotted x)   = "dotted " ++ (show (Basic x))
  show (Other x)    = "duration: " ++ (show x)


------------------------
-- Exported functions --
------------------------

-- | Tell if a duration is one of a regular note.
isNote :: Duration -> Bool
isNote (Other _) = False
isNote _         = True

-- | Inject the string formatting of a duration in a function taking this
-- string and returning tho Lilypond string of a note.
showNote :: Duration -> (String -> String) -> String
showNote n@(Other  _)          = flip ($) $ lilypond n
showNote n@(Basic  x) | x >= 0 = flip ($) $ lilypond n
showNote n@(Dotted x) | x >= 0 = flip ($) $ lilypond n
showNote n = const $ "R1 * " ++ (show $ (numerator $ duration n :: Integer))
-- TODO: long non-rest notes

-- | Get the closest Duration corresponding to a fraction of a measure.
fromFractional :: (Fractional a, Ord a, Real a) => a -> Duration
fromFractional x
  | x <= 0    = Other (toRational x)
  | x <= 1    = search succ $ Basic 0
  | otherwise = search pred $ Basic 0
  where
    search next i
      | diff i <= diff (next i) = i
      | otherwise               = search next $ next i
    diff                        = abs . (subtract x) . duration

guess :: (Fractional a, Ord a, Real a) => a -> [PossibleDuration]
guess time = [struct (pred closest), struct closest, struct (succ closest)]
  where
    closest  = fromFractional time
    struct d = PossibleDuration d (toRational time) $
                 toRational $ abs $ 1 - (time / (duration d))

---------------------
-- Local functions --
---------------------

-- Give the fraction of a measure corresponding to a Duration.
duration :: Fractional a => Duration -> a
duration (Basic  x) = 1 / 2 ^^ x
duration (Dotted x) = 3 / 2 ^^ (x + 1)
duration (Other  x) = fromRational x

fromFractional' :: (Fractional a, Ord a, Real a) => a -> Duration
fromFractional' x = if (x == duration try) then try else Other $ toRational x
  where try = fromFractional x

-- Return the Lilypond suffix corresponding to the duration.
lilypond :: Duration -> String
lilypond (Basic  x) = show $ (2 :: Integer) ^ (max x 0) -- TODO: x < 0
lilypond (Dotted x) = lilypond (Basic x) ++ "."
lilypond (Other  _) = "0"
