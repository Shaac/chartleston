module Structure (structure) where

import Data.List  (partition)
import Data.Ratio (denominator)

import Duration (Duration (Other), duration)
import Note     (Note, isCymbal, isTom, flams)

structure :: [(Duration, [Note])] ->
  [([(Duration, [Note])], [(Duration, [Note])])]
structure = map voices . measures . getFlams

getFlams :: [(a, [Note])] -> [(a, [Note])]
getFlams = map (\(x, y) -> (x, flams y))

measures :: [(Duration, a)] -> [[(Duration, a)]]
measures = measure (0 :: Rational)
  where
    measure _   []            = []
    measure acc (x@(d, _):xs) = let s = (duration d + acc) in
      if denominator s == 1
        then [x] : (measure 0 xs)
        else let l = measure s xs in
          if length l == 0 then [x] : (measure 0 xs) -- ERR: malformed measure!
          else (x : (head l)) : (tail l)

-- Separate the notes in two voices. The cymbals are up, and the rest down.
voices :: [(Duration, [Note])] -> ([(Duration, [Note])], [(Duration, [Note])])
voices x = (removeRests $ zip time xs, removeRests $ zip time ys)
  where
    (xs', ys')    = unzip $ map (partition isCymbal) notes
    (xs, ys)      = if null xs' then unzip $ map (partition isTom) notes
                    else (xs', ys')
    (time, notes) = unzip x

-- Remove the rests on a voices by making previous notes longer.
removeRests :: (Eq a) => [(Duration, [a])] -> [(Duration, [a])]
removeRests = converge . (iterate $ aux (0 :: Rational))
  where
    converge (x:y:xs) = if x == y then x else converge (y : xs)
    converge _        = fail "This can not occure."
    aux e ((t1, x):(t2, []):xs)
      | t1 + t2 /= Other && ok t1 e = (t1 + t2, x) : (aux (add (add e t1) t2) xs)
      | otherwise     = (t1, x) : (aux (add e t1) $ (t2, []) : xs)
    aux e ((t, x):xs) = (t, x) : (aux (add e t) xs)
    aux _ []          = []
    add e x           = duration x + e
    ok t e = if t' > denominator e then True else t' <= denominator (add e t)
      where t' = denominator $ duration t
