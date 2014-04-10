import System.Environment (getArgs)

import Analyse (analyse)
import Midi    (open)
import Write   (write)

main :: IO ()
main = do
    args <- getArgs
    midi <- open $ args !! 0
    writeFile (arg args "o") $ write $ analyse $ midi

-- Read parameter from command line arguments.
arg :: [String] -> String -> String
arg []       _   = ""
arg [_]      _   = ""
arg (x:xs)   s
  | x == '-' : s = head xs
  | otherwise    = arg xs s
