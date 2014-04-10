module Analyse (analyse) where

import Data.List (group, sort)

-- | Use all below functions.
analyse :: [(Integer, a)] -> [(Integer, [a])]
analyse = uncurry zip . (mapFst treat) . unzip . shiftFst . join
  where
    treat = lastNote . detect . equalise
    shiftFst = uncurry zip . (mapFst (drop 1 . cycle)) . unzip

-- Use a simple but crude tempo detection. To be used after a pre-treatmnent.
detect :: [Integer] -> [Integer]
detect xs = map ((2 ^) . closest . divide . fromInteger . (max 1)) xs
  where
    closest  = max 0 . round . (/ log 2) . log     :: Double -> Integer
    divide   = (8 * (fromInteger $ majority xs) /) :: Double -> Double
    majority = snd . maximum . (map (\x -> (length x, head x))) . group . sort

-- Equalise an integer list: close values next to each other are leveled.
equalise :: [Integer] -> [Integer]
equalise [] = []
equalise xs = let (similar, rest) = cut xs in level similar ++ (equalise rest)
  where
    cut []     = ([], []) -- Not a possible input, but avoid warnings.
    cut (y:ys) = mapFst (y:) $ span ((< 0.2) . (ratio y)) ys
    ratio y x  = abs $ fromInteger x / (fromInteger y) - 1 :: Rational
    level l    = let s = length l in replicate s $ sum l `div` (fromIntegral s)

-- Join notes that are close into simultaneous notes.
join :: [(Integer, a)] -> [(Integer, [a])]
join = mergeZeros (0, []) . (setZeros 0)
  where
    -- Set close to zero numbers to zero, and add that time to next note.
    setZeros _ []   = []
    setZeros t ((time, note):xs)
      | time < 20 = (0, note) : (setZeros (time + t) xs)
      | otherwise = (time + t, note) : (setZeros 0 xs)
    -- Merge notes with a time of zero with the previous one.
    mergeZeros x [] = [x]
    mergeZeros (time, note) ((0, x):xs) = mergeZeros (time, x : note) xs
    mergeZeros x ((x1, x2):xs) = x : (mergeZeros (x1, [x2]) xs)

-- Give a duration to the last note, so that it last until the end of a mesure.
lastNote :: [Integer] -> [Integer]
lastNote = aux 0
  where
    aux _   []     = fail "There is no data."
    aux acc [_]    = [round $ 1 / (1 - dec)]
      where dec = snd (properFraction acc :: (Integer, Rational))
    aux acc (x:xs) = x : (aux (1 / (fromInteger x) + acc) xs)

-- Apply a function to the first item of a tuple.
mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (x, y) = (f x, y)
