module Duration (Duration(Other), fromFractional, duration, showNote) where

import Data.Ratio (numerator)

---------------
-- Structure --
---------------

-- Basic: The note length is 1 / 2^n of that of the measure.
-- Dotted: The note lengt is 1.5 times that of a basic note.
data Duration = Basic Integer | Dotted Integer | Other deriving Eq

instance Num Duration where
  -- | Two notes can be added if the sum is a regular note.
  (Basic x) + (Basic y)
    | x == y     = Basic (x - 1)
    | x == y + 1 = Dotted y
    | x + 1 == y = Dotted x
    | otherwise  = Other
  (Basic x) + (Dotted y)
    | x == y + 1 = Basic (y - 1)
    | otherwise = Other
  (Dotted x) + (Dotted y)
    | x == y = Dotted (x - 1)
    | otherwise = Other
  Other + _ = Other
  a + b = b + a

  -- | Soustraction of two notes will probably never be used but still.
  (Basic x) - (Basic y)
    | x + 1 == y = Basic y
    | x + 2 == y = Dotted (y + 1)
    | otherwise  = Other
  (Dotted x) - (Basic y)
    | x + 1 == y = Basic x
    | otherwise  = Other
  (Basic x) - (Dotted y)
    | x + 1 == y = Basic (y - 1)
    | otherwise  = Other
  (Dotted x) - (Dotted y)
    | x + 1 == y = Dotted y
    | otherwise  = Other
  _ - _ = Other

  -- These are required for Num instance, but have no meaning here.
  _ * _ = error "Multiplication has no meaning."
  signum _ = 1
  abs = id

  -- | A bijection is defined between durations and integers.
  fromInteger x
    | x `mod` 2 == 0 = Basic (x `div` 2)
    | otherwise      = Dotted ((x + 1) `div` 2)

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
  show Other        = "unknown duration"


------------------------
-- Exported functions --
------------------------

-- | Inject the string formatting of a duration in a function taking this
-- string and returning tho Lilypond string of a note.
showNote :: Duration -> (String -> String) -> String
showNote Other                 = flip ($) $ lilypond Other
showNote n@(Basic  x) | x >= 0 = flip ($) $ lilypond n
showNote n@(Dotted x) | x >= 0 = flip ($) $ lilypond n
showNote n = const $ "R1 * " ++ (show $ (numerator $ duration n :: Integer))
-- TODO: long non-rest notes

-- | Give the fraction of a measure corresponding to a Duration.
duration :: Fractional a => Duration -> a
duration (Basic  x) = 1 / 2 ^^ x
duration (Dotted x) = 1 / 2 ^^ x + 1 / 2 ^^ (x + 1)
duration _          = error "No duration."

-- | Get the closest Duration corresponding to a fraction of a measure.
fromFractional :: (Fractional a, Ord a) => a -> Duration
fromFractional x
  | x <= 0    = Other
  | x <= 1    = search succ 0
  | otherwise = search pred 0
  where
    search next i
      | diff i <= diff (next i) = fromInteger i
      | otherwise               = search next $ next i
    diff                        = abs . (subtract x) . duration . fromInteger


---------------------
-- Local functions --
---------------------

-- Return the Lilypond suffix corresponding to the duration.
lilypond :: Duration -> String
lilypond (Basic  x) = show $ (2 :: Integer) ^ (max x 0) -- TODO: x < 0
lilypond (Dotted x) = lilypond (Basic x) ++ "."
lilypond Other      = "0"
