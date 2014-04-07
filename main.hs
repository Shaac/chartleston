import System.Environment (getArgs)

import Analyse (analyse)
import Midi    (open)
import Write   (write)

main :: IO ()
main = do
    args <- getArgs
    midi <- open $ args !! 0
    putStr $ write $ analyse $ midi
