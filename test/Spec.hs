module Spec (main) where

import Data.Char (isSpace)
import Data.List (isInfixOf)
import Test.Hspec (describe)
import qualified Test.Hspec as Hspec
import Test.Hspec.QuickCheck (prop)
import Trimdent (trim)

main :: IO ()
main = Hspec.hspec tests

tests :: Hspec.Spec
tests = do
  describe "trim" $ do
    prop "preservesContent" prop_trim_preservesContent
    prop "commutesWithReverse" prop_trim_commutesWithReverse
    prop "resultDoesntStartWithWhitespace" prop_trim_removesStartSpace

prop_trim_preservesContent :: String -> Bool
prop_trim_preservesContent s = trim s `isInfixOf` s

prop_trim_commutesWithReverse :: String -> Bool
prop_trim_commutesWithReverse s = (trim . reverse) s == (reverse . trim) s

prop_trim_removesStartSpace :: String -> Bool
prop_trim_removesStartSpace s =
  case trim s of
    "" -> True
    c : _ -> not $ isSpace c
