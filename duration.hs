module Duration (Duration, fromFractional) where

-- Basic: The note length is 1 / 2^n of that of the measure.
-- Dotted: The note lengt is 1.5 times that of a basic note.
data Duration = Basic Integer | Dotted Integer | Other

instance Num Duration where
  (Basic x) + (Basic y)
    | x == y     = Basic (x - 1)
    | x == y + 1 = Dotted y
    | x + 1 == y = Dotted x
    | otherwise  = Other
  (Basic x) + (Dotted y)
    | x + 1 == y = Basic (y - 1)
    | otherwise = Other
  (Dotted x) + (Dotted y)
    | x == y = Dotted (x - 1)
    | otherwise = Other
  Other + _ = Other

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
  Other - Other = Other

  _ * _ = error "Multiplication has no meaning."
  signum _ = 1
  abs = id

  fromInteger x
    | x `mod` 2 == 0 = Basic (x `div` 2)
    | otherwise      = Dotted ((x + 1) `div` 2)

instance Show Duration where
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

instance Eq Duration where
  (Basic x)  == (Basic y)  = x == y
  (Dotted x) == (Dotted y) = x == y
  _          == _          = False

duration :: Fractional a => Duration -> a
duration (Basic x)  = 1 / 2 ^^ x
duration (Dotted x) = 1 / 2 ^^ x + 1 / 2 ^^ (x + 1)
duration _          = error "No duration."

fromFractional :: (Fractional a, Ord a) => a -> Duration
fromFractional x
  | x == 0 = Other
  | x <= 1 = aux succ 0
  | x >  1 = aux pred 0
  where
    aux f i
      | diff i <= diff (f i) = fromInteger i
      | otherwise            = aux f (f i)
    diff i = abs (duration (fromInteger i) - x)
