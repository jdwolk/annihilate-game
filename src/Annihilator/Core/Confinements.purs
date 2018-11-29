module Annihilator.Core.Confinements where

import Prelude
import Data.Foldable (and, all)
import Data.Maybe (Maybe(..), isJust, fromJust)
import Data.Array (head, length, filter)
import Partial.Unsafe (unsafePartial)
import Annihilator.Core.Cards

allRegular = all qcIsRegular
allAnti = all qcIsAnti

-- TODO: check if the count of one color is the same as the length of qcs
allSameColor qcs = result where
  maybeFirstColor = join $ qcColor <$> (head qcs)
  allColor maybeColor xs = all (\qc -> qcColor qc == maybeColor) xs
  result = allColor maybeFirstColor qcs

oneOfEachColor qcs = all onlyOnce colors where
  colorsMatching c = filter ((==) c)
  colors = map (\x -> unsafePartial $ fromJust x) $ filter isJust $ map qcColor qcs
  onlyOnce color = (==) 1 $ length $ colorsMatching color colors

lengthIs n xs = (length xs) == n

isValidConfinement qcs = (lengthIs 3 qcs && allRegular qcs && oneOfEachColor qcs)
                      || (lengthIs 3 qcs && allAnti qcs && oneOfEachColor qcs)
                      || (lengthIs 2 qcs && allSameColor qcs)
