module Score (Measure (..), Score (..)) where

import Duration (Duration)
import Note     (Note)

data Score a = Score {
    title :: String,
    score :: a
}

instance Functor Score where
  fmap f s = s { score = f (score s) }

data Measure = Measure {
    measure :: ([(Duration, [Note])], [(Duration, [Note])]),
    nRepeat :: Int
} deriving Show
