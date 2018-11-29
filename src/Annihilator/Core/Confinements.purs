module Annihilator.Core.Confinements where

import Prelude
import Data.Foldable (all)
import Data.Array (head, filter, length)
import Annihilator.Core.Cards (Card, qcColor, qcColors, qcIsRegular, qcIsAnti, allColors)

allRegular :: Array Card -> Boolean
allRegular = all qcIsRegular

allAnti :: Array Card -> Boolean
allAnti = all qcIsAnti

-- TODO: check if the count of one color is the same as the length of qcs
allSameColor :: Array Card -> Boolean
allSameColor qcs = result where
  maybeFirstColor = join $ qcColor <$> (head qcs)
  allColor maybeColor xs = all (\qc -> qcColor qc == maybeColor) xs
  result = allColor maybeFirstColor qcs

oneOfEachColor :: Array Card -> Boolean
oneOfEachColor cards = all onlyOnce allColors where
  colorsMatching c = filter ((==) c)
  onlyOnce color = (==) 1 $ length $ colorsMatching color $ qcColors cards

lengthIs :: forall a . Int -> Array a -> Boolean
lengthIs n xs = (length xs) == n

isValidConfinement :: Array Card -> Boolean
isValidConfinement cards = (lengthIs 3 cards && allRegular cards && oneOfEachColor cards)
                      || (lengthIs 3 cards && allAnti cards && oneOfEachColor cards)
                      || (lengthIs 2 cards && allSameColor cards)
