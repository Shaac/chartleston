module Analyse where

import Midi (Note)

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
