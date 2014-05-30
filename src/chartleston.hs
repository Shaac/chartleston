import Data.Char          (toUpper)
import System.Environment (getArgs)

import Analyse   (analyse)
import Midi      (open)
import Structure (structure)
import Write     (write)

-------------------
-- Main function --
-------------------

-- | Read a midi file name from the command line and create its corresponding
-- sheet music.
main :: IO ()
main = do
  args <- getArgs
  midi <- open $ args !! 0
  if map toUpper (arg args "debug") == "TRUE" then do
    putStrLn $ show $ midi
    putStrLn $ show $ analyse $ midi
    putStrLn $ show $ structure $ analyse $ midi
  else
    writeFile (output args) $ write $ structure $ analyse $ midi


---------------------
-- Local functions --
---------------------

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