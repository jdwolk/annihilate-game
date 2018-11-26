module Annihilator.Core.Confinements where

import Prelude
import Data.Foldable (and, all)
import Data.Maybe (isJust)
import Data.Array (head, length)
import Annihilator.Core.Cards

allRegular = all qcIsRegular
allAnti = all qcIsAnti
allSameColor qcs = result where
  maybeFirstColor = join $ qcColor <$> (head qcs)
  allColor maybeColor xs = all (\qc -> qcColor qc == maybeColor) xs
  result = allColor maybeFirstColor qcs

lengthIs n xs = (length xs) == n

isValidConfinement qcs = (lengthIs 3 qcs && allRegular qcs)
                      || (lengthIs 3 qcs && allAnti qcs)
                      || (lengthIs 2 qcs && allSameColor qcs)
