module Structure (measures) where

import Data.Ratio (denominator)

import Duration (Duration, duration)

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
