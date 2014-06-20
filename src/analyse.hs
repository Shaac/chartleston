module Analyse (analyse) where

import Data.List     (group, sort)
import Control.Arrow (first, (***), (&&&))

import Duration (Duration, guess)


---------------
-- Constants --
---------------

-- Maximal ratio of two durations for them to be equalised.
equal :: Rational
equal = 0.2

-- Minimal duration, in seconds, of the silence at the beginning.
minimalRest :: Rational
minimalRest = 0.5

-- Duration, in seconds, of an ideal semibreve.
semibreve :: Rational
semibreve = 3

-- Maximal duration, in seconds, of two simultaneous notes.
simultaneous :: Rational
simultaneous = 0.075


------------------------
-- Exported functions --
------------------------

-- | Analyse a list of real notes and organise them with regular durations.
analyse :: (RealFrac a, Ord a) => [(a, b)] -> [(Duration, [b])]
analyse = uncurry zip . first treat . unzip . join
  where treat = lastNote . detect . normalise . equalise . drop 1


---------------------
-- Local functions --
---------------------

-- Give a duration to last note, so that it lasts until the end of a mesure.
lastNote :: RealFrac a => [a] -> [a]
lastNote xs = xs ++ [1 - ((snd :: (Int, a) -> a) $ properFraction $ sum xs)]

-- Detect the notes durations.
detect :: RealFrac a => [(a, a)] -> [Duration]
detect = guess . map snd

-- Normalise the duration list so that it represents the fraction of a measure.
normalise :: RealFrac a => [(a, a)] -> [(a, a)]
normalise xs = map ((/ norm) *** (/ norm)) xs
  where
    norm     = let x = majority xs in closest (fromRational semibreve / x) * x
    majority = snd . maximum . map (length &&& head) . group . sort . map fst
    closest x
      | x <= 2    = max 1 $ fromInteger $ round x
      | otherwise = 2 * (closest (x / 2))

-- Equalise an integer list: close values next to each other are leveled.
-- The original values are kept aside.
equalise :: (Fractional a, Ord a) => [a] -> [(a, a)]
equalise durations = zip (equalise' durations) durations
  where
    equalise' [] = []
    equalise' xs = uncurry (++) $ level *** equalise' $ cut xs
    cut       xs = span ((< fromRational equal) . abs . (1 -) . (/ head xs)) xs
    level     xs = let s = length xs in replicate s $ sum xs / (fromIntegral s)

-- Join notes that are close into simultaneous notes.
join :: (Fractional a, Ord a) => [(a, b)] -> [(a, [b])]
join = initialRest . join'
  where
    initialRest xs -- If the first note is a substantial rest, add it.
      | null $ takeWhile ((> fromRational minimalRest) . fst) xs = xs
      | otherwise                                                = (0, []) : xs
    join' []            = []
    join' (x : xs)      = ((+ offset) *** (: map snd simult)) x : join' rest
      where
        (simult, after) = span ((< fromRational simultaneous) . fst) xs
        rest = case after of
          []            -> []
          (y : ys)      -> first (+ offset) y : ys
        offset          = (/ 2) $ sum $ map fst simult
