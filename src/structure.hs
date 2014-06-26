module Structure (structure) where

import Control.Arrow (first, second, (&&&))
import Data.List     (partition)
import Data.Ratio    (denominator)

import Duration (Duration, isNote)
import Note     (Note, isCymbal, isTom, flams, pedals)
import Score    (Measures (..))


------------------------
-- Exported functions --
------------------------

-- | Get the structure of a notes list.
structure :: [(Duration, [Note])] -> [(Measures, Int)]
structure = repeats . map voices . measures . map (second $ pedals . flams)


---------------------
-- Local functions --
---------------------

-- Regroup the notes by measure.
measures :: [(Duration, a)] -> [[(Duration, a)]]
measures = measure (0 :: Rational)
  where
    measure _   []              = []
    measure acc (x@(d, _) : xs) = let s = toRational d + acc in
      if denominator s == 1
        then [x] : measure 0 xs
        else let l = measure s xs in
          if null l then [x] : measure 0 xs -- ERR: malformed measure!
          else (x : head l) : tail l

-- Separate the notes in two voices. The cymbals are up, and the rest down.
voices :: [(Duration, [Note])] -> ([(Duration, [Note])], [(Duration, [Note])])
voices x = (flip ($) xs &&& flip ($) ys) (removeRests . zip time)
  where
    (xs', ys')    = unzip $ map (partition isCymbal) notes
    (xs, ys)      = if null xs' then unzip $ map (partition isTom) notes
                    else (xs', ys')
    (time, notes) = unzip x

-- Remove the rests on a voices by making previous notes longer.
removeRests :: (Eq a) => [(Duration, [a])] -> [(Duration, [a])]
removeRests = converge . (iterate $ aux (0 :: Rational))
  where
    converge (x : y : xs) = if x == y then x else converge $ y : xs
    converge _            = fail "This can not occure."
    aux e (n@(t1, x) : (t2, []) : xs)
      | isNote (t1 + t2) && ok n e = (t1 + t2, x) : aux (add (add e t1) t2) xs
      | otherwise         = (t1, x) : (aux (add e t1) $ (t2, []) : xs)
    aux e ((t, x) : xs)   = (t, x) : (aux (add e t) xs)
    aux _ []              = []
    add e x               = toRational x + e
    ok (t, l) e = if not (null l) && toRational t >= 0.25 then False else
      if t' > denominator e then True else t' <= denominator (add e t)
      where t' = denominator $ toRational t

-- Regroup equal measures that are next to each other.
repeats :: [([(Duration, [Note])], [(Duration, [Note])])] -> [(Measures, Int)]
repeats = ds . volta . map (first Simple) . double . simple . map start
  where
    start x = ([x], 1)
    simple ((a, na) : t@((b, nb) : xs))
      | a == b           = simple $ (a, na + nb) : xs
      | otherwise        = (a, na) : simple t
    simple a             = a
    double ((a, 1) : (b, 1) : (c, 1) : (d, 1) : xs)
      | a == c && b == d = double $ (a ++ b, 2) : xs
    double (([a, b], na) : ([c], 1) : ([d], 1) : xs)
      | a == c && b == d = double $ ([a, b], na + 1) : xs
    double (a : xs)      = a : double xs
    double []            = []
    volta ((Simple [a, b], n) : (Simple [c], 1) : (Simple [d], 1) : xs)
      | a == c           = (Volta (a, b, d), n + 1) : volta xs
    volta ((Simple [a], 1):(Simple [b], 1):(Simple [c], 1):(Simple [d], 1):xs)
      | a == c           = (Volta (a, b, d), 2) : volta xs
    volta (a : xs)       = a : volta xs
    volta []             = []
    ds []                = []
    ds (x : xs) = case ds' [x] xs of
      0 -> x : ds xs
      n -> uncurry (:) $ first (DalSegno &&& const 1) $ splitAt n $ drop (n - 1) xs
    ds' _ [] = 0
    ds' h xs
      | null b                               = 0
      | all (\x -> fst x == snd x) (zip a b) = length a
      | otherwise                            = ds' a b
      where (a, b) = first (h ++) $ span (/= head h) xs
