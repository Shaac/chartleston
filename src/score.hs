module Score (Measure (..), Score (..)) where

import Duration (Duration)
import Note     (Note)


---------------
-- Structure --
---------------

-- Represent the work in progress.
data Score a = Score {
    title :: String,
    score :: a
}

instance Functor Score where
  fmap f s = s { score = f (score s) }

-- Represent a measure, and how it is repeated.
data Measure = Measure {
    measure :: ([(Duration, [Note])], [(Duration, [Note])]),
    nRepeat :: Int
} deriving Show
