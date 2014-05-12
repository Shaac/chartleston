import Data.Char          (toUpper)
import System.Environment (getArgs)

import Analyse   (analyse)
import Midi      (open)
import Structure (measures)
import Write     (write)

main :: IO ()
main = do
    args <- getArgs
    midi <- open $ args !! 0
    writeFile (output args) $ write $ measures $ analyse $ midi

-- Read parameter from command line arguments.
arg :: [String] -> String -> String
arg []       _   = ""
arg [_]      _   = ""
arg (x:xs)   s
  | x == '-' : s = head xs
  | otherwise    = arg xs s

-- Get the output filename.
output :: [String] -> String
output args = if param /= "" then param else ly "" (args !! 0)
  where
    param   = arg args "o"
    ly acc []       = acc ++ ".ly"
    ly _   ('/':xs) = ly "" xs
    ly acc (x:xs)
      | x == '.' && map toUpper xs == "MID"  = acc ++ ".ly"
      | x == '.' && map toUpper xs == "MIDI" = acc ++ ".ly"
      | otherwise                            = ly (acc ++ [x]) xs
