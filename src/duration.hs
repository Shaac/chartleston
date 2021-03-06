module Duration (Duration, guess, isNote, showNote) where

import Control.Arrow (second, (&&&))
import Control.Monad (liftM)
import Data.Maybe    (mapMaybe)
import Data.Ratio    (numerator)
import GHC.Exts      (sortWith)


---------------
-- Constants --
---------------

-- Number of simultaneous guesses for the detection.
guesses :: Int
guesses = 20

-- Coefficient used to amplify the error in old detected measures.
-- Should be greater than 1 to move on to the current measure.
old :: Rational
old = 1.1

---------------
-- Structure --
---------------

-- Basic: The note length is 1 / 2^n of that of the measure.
-- Dotted: The note lengt is 1.5 times that of a basic note.
data Duration = Basic Integer | Dotted Integer | Other Rational deriving Eq

-- Represent a possible guess for a duration.
data PossibleDuration = PossibleDuration {
    value    :: Duration, -- Guessed value.
    original :: Rational, -- Original value.
    err      :: Rational  -- Error between original and guessed values.
} deriving Show

-- Represent a possible guess for an entire score.
data PossibleDurations = PossibleDurations {
    ok      :: [PossibleDuration], -- Already checked values.
    okErr   :: Rational,           -- Error for the checked values.
    current :: [PossibleDuration]  -- Values worked on.
} deriving Show

-- A duration is a measure of time, so it can be added.
instance Num Duration where
  -- | Two notes can be added if the sum is a regular note.
  a@(Basic x) + b@(Basic y)
    | x == y     = Basic $ x - 1
    | x == y + 1 = Dotted y
    | x + 1 == y = Dotted x
    | otherwise  = Other $ duration a + duration b
  a@(Basic x) + b@(Dotted y)
    | x == y + 1 = Basic $ y - 1
    | otherwise = Other $ duration a + duration b
  a@(Dotted x) + b@(Dotted y)
    | x == y = Dotted $ x - 1
    | otherwise = Other $ duration a + duration b
  (Other r) + x = closest' $ r + duration x
  a + b = b + a

  -- | Soustraction of two notes will probably never be used but still.
  a@(Basic x) - b@(Basic y)
    | x + 1 == y = Basic y
    | x + 2 == y = Dotted $ y + 1
    | otherwise  = Other $ duration a - duration b
  a@(Dotted x) - b@(Basic y)
    | x + 1 == y = Basic x
    | otherwise  = Other $ duration a - duration b
  a@(Basic x) - b@(Dotted y)
    | x + 1 == y = Basic $ y + 1
    | otherwise  = Other $ duration a - duration b
  a@(Dotted x) - b@(Dotted y)
    | x + 1 == y = Dotted y
    | otherwise  = Other $ duration a - duration b
  a - b          = closest' (duration a - duration b :: Rational)

  (Basic  x) * (Basic  y) = Basic    $ x + y
  (Basic  x) * (Dotted y) = Dotted   $ x + y
  (Dotted x) * (Dotted y) = Dotted   $ x + y - 1
  (Other  x) * d          = closest' $ x + duration d
  a          * b          = b * a

  signum (Other x)
    | x == 0       = Other 0
    | x >  0       = Basic 0
    | x <  0       = Other $ - 1
  signum _         = Basic 0

  abs (Other x) = Other $ abs x
  abs d         = d

  fromInteger = closest' . (fromInteger :: Integer -> Rational)

-- A duration has a previous and a next value (eg. dotted notes).
instance Enum Duration where
  toEnum i
    | i < 7 && i `mod` 2 == 0 = Basic  $  toInteger i      `div` 2
    | i < 7                   = Dotted $ (toInteger i + 1) `div` 2
    | otherwise               = Basic  $ toInteger $ i - 3

  fromEnum (Basic  x)
    | x < 4     = fromInteger $ x * 2
    | otherwise = fromInteger $ x + 3
  fromEnum (Dotted x)
    | x < 4     = fromInteger $ x * 2 - 1
    | otherwise = error "Such small dotted can not be an Enum."
  fromEnum _    = error "Other can not be an Enum."

-- A duration is longer or shorter than an other.
instance Ord Duration where
  a <= b = (duration a :: Rational) <= (duration b :: Rational)

-- A duration represents a fraction of a semibreve.
instance Fractional Duration where
  recip   (Basic  x) = Basic    $ - x
  recip n@(Dotted _) = Other    $ recip $ duration n
  recip   (Other  x) = closest' $ recip x

  fromRational = closest'

instance Real Duration where
  toRational = duration

instance RealFrac Duration where
  properFraction = second closest' . properFraction . toRational

instance Show Duration where
  -- Display the duration name in UK English.
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
  show (Dotted x)   = "dotted " ++ (show $ Basic x)
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
showNote n = const $ "R1 * " ++ (show (numerator $ duration n :: Integer))
-- TODO: long non-rest notes

guess :: (Fractional a, Real a) => [a] -> [Duration]
guess = bestGuess . foldl (flip guessNext) [PossibleDurations [] 0 []]
  where bestGuess = map value . (\x -> ok x ++ current x) . head . keep 1


---------------------
-- Local functions --
---------------------

-- Get the closest Duration corresponding to a fraction of a measure.
closest :: (Fractional a, Real a) => a -> Duration
closest x
  | x <= 0    = Other $ toRational x
  | x <= 1    = search succ $ Basic 0
  | otherwise = search pred $ Basic 0
  where
    search next i
      | diff i <= diff (next i) = i
      | otherwise               = search next $ next i
    diff                        = abs . subtract x . duration

-- See if the duration in fraction is an exact Duration.
closest' :: (Fractional a, Ord a, Real a) => a -> Duration
closest' x = if x == duration try then try else Other $ toRational x
  where try = closest x

-- Guess a new note, and add it to the already guessed.
guessNext :: (Fractional a, Real a) =>
  a -> [PossibleDurations] -> [PossibleDurations]
guessNext = (keep guesses .) . concatMap . flip add . guessOne
  where
    add x = map $ PossibleDurations (ok x) (okErr x) . (current x ++) . (: [])
    guessOne time = map (possible time) $ take 3 [pred (closest time)..]
    possible t d  = PossibleDuration d (toRational t) $ erro t $ duration d

-- Compute the error between a guessed note and its actual value.
erro :: (Fractional a, Real a) => a -> a -> Rational
erro _ 0 = 0
erro a b = toRational $ abs $ 1 - (a / b)

-- Compute the error of a group of guessed notes.
err' :: [PossibleDuration] -> Rational
err' xs = fold err xs * (erro (fold original xs) $ fold (duration . value) xs)
  where fold f = foldr ((+) . f) 0

-- Keep only the best guesses.
keep :: Int -> [PossibleDurations] -> [PossibleDurations]
keep n = map snd . take n . sortWith fst . map compute . matchTempo
  where compute = (uncurry (+) . (okErr &&& err' . current)) &&& id

-- Verify that a sequence of notes has no length longer than a quaver.
matchTempo :: [PossibleDurations] -> [PossibleDurations]
matchTempo l = if null result then finish l else result
  where
    finish        = id
    result        = mapMaybe remove l
    remove  xs    = liftM (aux xs) $ split (0 :: Rational) [] $ current xs
    aux xs (a, b) = PossibleDurations (ok xs ++ a) (err' a + old * okErr xs) b
    split a acc xs
      | a == 0.25 = Just (reverse acc, xs)
      | a >  0.25 = if (duration $ value $ head acc) > (0.25 :: Rational)
                    then split (a - 0.25) acc xs else Nothing
      | otherwise = case xs of
        []        -> Just ([], reverse acc)
        (x : xs') -> split (a + (duration $ value $ x)) (x : acc) xs'

-- Give the fraction of a measure corresponding to a Duration.
duration :: Fractional a => Duration -> a
duration (Basic  x) = 1 / 2 ^^ x
duration (Dotted x) = 3 / 2 ^^ (x + 1)
duration (Other  x) = fromRational x

-- Return the Lilypond suffix corresponding to the duration.
lilypond :: Duration -> String
lilypond (Basic  x) = show $ (2 :: Integer) ^ (max x 0)
lilypond (Dotted x) = lilypond (Basic x) ++ "."
lilypond (Other  _) = "0"
