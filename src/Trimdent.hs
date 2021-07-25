module Trimdent (
  trim,
) where

import Data.Char (isSpace)

-- | Trims whitespace characters from both ends of the text.
--
-- >>> trim " hello "
-- "hello"
trim :: String -> String
trim = dropWhileRev isSpace . dropWhile isSpace

dropWhileRev :: (Char -> Bool) -> String -> String
dropWhileRev p = reverse . dropWhile p . reverse
