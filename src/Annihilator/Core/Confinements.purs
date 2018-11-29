module Annihilator.Core.Confinements where

import Prelude
import Data.Foldable (all, any)
import Data.Array (filter, length)
import Annihilator.Core.Cards (Card, Color, qcColor, qcColors, qcIsRegular, qcIsAnti, allColors)

allRegular :: Array Card -> Boolean
allRegular = all qcIsRegular

allAnti :: Array Card -> Boolean
allAnti = all qcIsAnti

colorsMatching :: Color -> Array Card -> Array Color
colorsMatching color cards = filter ((==) color) $ qcColors cards

allSameColor :: Array Card -> Boolean
allSameColor cards = any allCardsHaveColor allColors where
  allCardsHaveColor color = (length cards) == (length $ colorsMatching color cards)

oneOfEachColor :: Array Card -> Boolean
oneOfEachColor cards = all onlyOnce allColors where
  onlyOnce color = (==) 1 $ length $ colorsMatching color cards

lengthIs :: forall a . Int -> Array a -> Boolean
lengthIs n xs = (length xs) == n

isValidConfinement :: Array Card -> Boolean
isValidConfinement cards = (lengthIs 3 cards && allRegular cards && oneOfEachColor cards)
                       || (lengthIs 3 cards && allAnti cards && oneOfEachColor cards)
                       || (lengthIs 2 cards && allSameColor cards)
