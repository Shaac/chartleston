module Analyse where

-- | Equalise an integer list: close values next to each other are leveled.
equalise :: [Integer] -> [Integer]
equalise [] = []
equalise xs = let (similar, rest) = cut xs in level similar ++ (equalise rest)
  where
    cut []     = ([], []) -- Not a possible input, but avoid warnings.
    cut (y:ys) = mapFst (y:) $ span ((< 0.2) . (ratio y)) ys
    ratio y x  = abs $ fromInteger x / (fromInteger y) - 1 :: Rational
    level l    = let s = length l in replicate s $ sum l `div` (fromIntegral s)

-- | Join notes that are close into simultaneous notes.
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

-- Shift the first values of the tuples, counterclokwise.
shiftFst :: [(a, b)] -> [(a, b)]
shiftFst = uncurry zip . (mapFst (drop 1 . cycle)) . unzip

-- Apply a function to the first item of a tuple.
mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (x, y) = (f x, y)
