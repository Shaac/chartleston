module Analyse (analyse) where

import Data.List (group, sort)

import Duration (Duration, fromFractional)


------------------------
-- Exported functions --
------------------------

-- | Analyse a list of real notes and organise them with regular durations.
analyse :: (RealFrac a, Ord a) => [(a, b)] -> [(Duration, [b])]
analyse = uncurry zip . (mapFst $ map fromFractional . treat) . unzip . join
  where
    treat = lastNote . detect . normalise . equalise . drop 1


---------------------
-- Local functions --
---------------------

mesures :: (Num a, Ord a) => [a] -> [[a]]
mesures [] = []
mesures ds = h : mesures q
  where
    (h, q)                    = sumWhile (<= 1) 0 ds
    sumWhile _ _   []         = ([], [])
    sumWhile f acc l@(x : xs)
      | f (acc + x)           = mapFst (x :) $ sumWhile f (acc + x) xs
      | otherwise             = if add x acc then ([x], xs) else ([], l)
    add note measure          = abs (1 - (note + measure)) < abs (1 - measure)

-- Give a duration to last note, so that it lasts until the end of a mesure.
lastNote :: RealFrac a => [a] -> [a]
lastNote xs = xs ++ [1 - ((snd :: (Int, a) -> a) $ properFraction $ sum xs)]

-- Use a simple but crude tempo detection. To be used after a pre-treatmnent.
detect :: RealFrac a => [(a, a)] -> [Rational]
detect = map (toRational . fromFractional) . (map fst)

-- Normalise the duration list so that it represents the fraction of a measure.
normalise :: RealFrac a => [(a, a)] -> [(a, a)]
normalise xs = map (\(a, b) -> (a / norm, b / norm)) xs
  where
    norm     = let x = majority (map fst xs) in x * (closest $ 3 / x)
    majority = snd . maximum . (map (\x -> (length x, head x))) . group . sort
    closest x
      | x <= 2    = max 1 $ fromInteger $ round x
      | otherwise = 2 * (closest (x / 2))

-- Equalise an integer list: close values next to each other are leveled.
-- The original values are kept aside.
equalise :: (Fractional a, Ord a) => [a] -> [(a, a)]
equalise durations = zip (aux durations) durations
  where
    aux []  = []
    aux xs  = let (similar, rest) = cut xs in level similar ++ (aux rest)
    cut l   = span ((< 0.2) . abs . (1 -) . (/ (head l))) l
    level l = let s = length l in replicate s $ sum l / (fromIntegral s)

-- Join notes that are close into simultaneous notes.
join :: (Fractional a, Ord a) => [(a, b)] -> [(a, [b])]
join = ((0, []) :) . aux
  where
    aux []            = []
    aux ((d, n) : xs) = (d + s, n : map snd simult) : aux rest
      where
        (simult, after) = span ((< 0.05) . fst) xs
        rest
          | null after  = []
          | otherwise   = mapFst (+ s) (head after) : tail after
        s               = (/ 2) $ sum $ map fst simult

-- Apply a function to the first item of a tuple.
mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (x, y) = (f x, y)
