module Analyse (analyse) where

import Data.List (group, sort)

import Duration (Duration, fromFractional, duration)


------------------------
-- Exported functions --
------------------------

-- | Analyse a list of real notes and organise them with regular durations.
analyse :: (RealFrac a, Ord a) => [(a, b)] -> [(Duration, [b])]
analyse = uncurry zip . (mapFst treat) . unzip . join
  where
    treat = lastNote . detect . equalise . (drop 1)


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
      | otherwise             = ([], l)

-- Use a simple but crude tempo detection. To be used after a pre-treatmnent.
detect :: (RealFrac a, Ord a) => [a] -> [Duration]
detect xs = map (fromFractional . (/ norm)) xs
  where
    norm     = let x = majority xs in x * fromInteger (closest $ 3 / x)
    majority = snd . maximum . (map (\x -> (length x, head x))) . group . sort
    closest x
      | x <= 2    = max 1 $ round x
      | otherwise = 2 * (closest (x / 2))


-- Equalise an integer list: close values next to each other are leveled.
equalise :: (Fractional a, Ord a) => [a] -> [a]
equalise []  = []
equalise xs  = let (similar, rest) = cut xs in level similar ++ (equalise rest)
  where
    cut l   = span ((< 0.2) . abs . (1 -) . (/ (head l))) l
    level l = let s = length l in replicate s $ sum l / (fromIntegral s)

-- Join notes that are close into simultaneous notes.
join :: (Fractional a, Ord a) => [(a, b)] -> [(a, [b])]
join = mergeZeros (0, []) . (setZeros 0)
  where
    -- Set close to zero numbers to zero, and add that time to next note.
    setZeros _ []   = []
    setZeros t ((time, note):xs)
      | time < 0.05 = (0, note) : (setZeros (time + t) xs)
      | otherwise = (time + t, note) : (setZeros 0 xs)
    -- Merge notes with a time of zero with the previous one.
    mergeZeros x [] = [x]
    mergeZeros (time, note) ((0, x):xs) = mergeZeros (time, x : note) xs
    mergeZeros x ((x1, x2):xs) = x : (mergeZeros (x1, [x2]) xs)

-- Give a duration to the last note, so that it last until the end of a mesure.
lastNote :: [Duration] -> [Duration]
lastNote = aux 0
  where
    aux acc (x:xs) = x : (aux (duration x + acc) xs)
    aux acc []     = [fromFractional $ 1 / (1 - dec)]
      where dec = snd (properFraction acc :: (Integer, Rational))

-- Apply a function to the first item of a tuple.
mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (x, y) = (f x, y)
