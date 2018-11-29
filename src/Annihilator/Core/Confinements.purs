module Annihilator.Core.Confinements where

import Prelude
import Data.Foldable (all, any, and, or)
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

applyToAll :: forall a b . Array (a -> b) -> a -> Array b
applyToAll fns a = map (\f -> f a) fns

allTrue :: forall a . Array (a -> Boolean) -> a -> Boolean
allTrue fs xs = and (applyToAll fs xs)

isValidConfinement :: Array Card -> Boolean
isValidConfinement cards =
  or [ (allTrue [lengthIs 3, allRegular, oneOfEachColor] cards)
     , (allTrue [lengthIs 3, allAnti, oneOfEachColor] cards)
     , (allTrue [lengthIs 2, allSameColor] cards)
     ]
