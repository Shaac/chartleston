module Analyse where

import Midi (Note)

equalise :: [Integer] -> [Integer]
equalise [] = []
equalise l = let (a, b) = split l in treat a ++ (equalise b)
  where
    split []     = ([], []) -- Not a possible input, but avoid warnings.
    split (x:xs) = mapFst (x:) $ span ((< 0.2) . (ratio x)) xs
    ratio x y    = abs $ fromInteger y / (fromInteger x) - 1 :: Rational
    treat x      = let s = length x in replicate s ((sum x) `div` (fromIntegral s))

join :: [(Integer, Note)] -> [(Integer, [Note])]
join = mergeZeros (0, []) . (setZeros 0)
  where
    setZeros _ []   = []
    setZeros t ((time, note):xs)
      | time < 20 = (0, note) : (setZeros (time + t) xs)
      | otherwise = (time + t, note) : (setZeros 0 xs)
    mergeZeros a [] = [a]
    mergeZeros (a1,a2) ((0,x):xs) = mergeZeros (a1, x:a2) xs
    mergeZeros a ((x1,x2):xs) = a : (mergeZeros (x1,[x2]) xs)

shiftFst :: [(a, b)] -> [(a, b)]
shiftFst = uncurry zip . (mapFst (drop 1 . cycle)) . unzip

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (x, y) = (f x, y)
