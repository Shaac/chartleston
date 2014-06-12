module Analyse (analyse) where

import Data.List     (group, sort)
import Control.Arrow (first, (***), (&&&))

import Duration (Duration, fromFractional, guess)


------------------------
-- Exported functions --
------------------------

-- | Analyse a list of real notes and organise them with regular durations.
analyse :: (RealFrac a, Ord a) => [(a, b)] -> [(Duration, [b])]
analyse = uncurry zip . (first $ map fromFractional . treat) . unzip . join
  where
    treat = lastNote . detect . normalise . equalise . drop 1


---------------------
-- Local functions --
---------------------

-- Give a duration to last note, so that it lasts until the end of a mesure.
lastNote :: RealFrac a => [a] -> [a]
lastNote xs = xs ++ [1 - ((snd :: (Int, a) -> a) $ properFraction $ sum xs)]

-- Detect the notes durations.
detect :: RealFrac a => [(a, a)] -> [Rational]
detect = map (toRational) . guess . map snd

-- Normalise the duration list so that it represents the fraction of a measure.
normalise :: RealFrac a => [(a, a)] -> [(a, a)]
normalise xs = map ((/ norm) *** (/ norm)) xs
  where
    norm     = let x = majority (map fst xs) in x * (closest $ 3 / x)
    majority = snd . maximum . map (length &&& head) . group . sort
    closest x
      | x <= 2    = max 1 $ fromInteger $ round x
      | otherwise = 2 * (closest (x / 2))

-- Equalise an integer list: close values next to each other are leveled.
-- The original values are kept aside.
equalise :: (Fractional a, Ord a) => [a] -> [(a, a)]
equalise durations = zip (equalise' durations) durations
  where
    equalise' [] = []
    equalise' xs = uncurry (($) . (++)) $ level *** equalise' $ cut xs
    cut       xs = span ((< 0.2) . abs . (1 -) . (/ (head xs))) xs
    level     xs = let s = length xs in replicate s $ sum xs / (fromIntegral s)

-- Join notes that are close into simultaneous notes.
join :: (Fractional a, Ord a) => [(a, b)] -> [(a, [b])]
join = initialRest . join'
  where
    initialRest xs -- If the first note is a substantial rest, add it.
      | null $ takeWhile ((> 0.5) . fst) xs = xs
      | otherwise                           = (0, []) : xs
    join' []            = []
    join' (x : xs)      = ((+ offset) *** (: map snd simult)) x : join' rest
      where
        (simult, after) = span ((< 0.075) . fst) xs
        rest = case after of
          []       -> []
          (y : ys) -> first (+ offset) y : ys
        offset          = (/ 2) $ sum $ map fst simult
