module Structure (measures) where

import Data.Ratio (denominator)

measures :: [(Integer, a)] -> [[(Integer, a)]]
measures = measure 0
  where
    measure _   []            = []
    measure acc (x@(t, _):xs) = let s = (1 / (fromInteger t) + acc) in
      if denominator s == 1
        then [x] : (measure 0 xs)
        else let (a : b) = measure s xs in (x : a) : b
