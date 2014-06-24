module Score (Measures (..), Score (..), showMeasures) where

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


type Measure = ([(Duration, [Note])], [(Duration, [Note])])

data Measures = Simple [Measure]
              | Volta (Measure, Measure, Measure)
              deriving Eq

showMeasures :: (Measure -> String) -> (Measures, Int) -> String
showMeasures s (Simple l, 1)        = concatMap s l
showMeasures s (Simple l, n)        = "        \\repeat percent " ++
                                      show n ++ " {\n" ++ concatMap s l
                                      ++ "        }\n"
showMeasures s (Volta (m, e, a), n) = "        \\repeat volta " ++
                                      show n ++ "\n" ++ s m ++
                                      "        \\alternative {\n" ++ s e
                                      ++ s a ++ "        }\n"
