module Annihilator.Cards where

import Prelude
import Data.Show (class Show)

class PrettyPrint a where
  pretty :: a -> String

data QuarkType = Up | Down | Bottom | Strange | Charm
data Color = Red | Green | Blue
data RegularOrAnti = Regular | Anti
data Quark =
  Quark { matterType :: RegularOrAnti
        , color :: Color
        , quarkType :: QuarkType
        }

derive instance eqQuarkType :: Eq QuarkType

derive instance eqColor :: Eq Color

derive instance eqRegularOrAnti :: Eq RegularOrAnti

derive instance eqQuark :: Eq Quark

instance showQuarkType :: Show QuarkType where
  show Up = "Up"
  show Down = "Down"
  show Bottom = "Bottom"
  show Strange = "Strange"
  show Charm = "Charm"

instance showColor :: Show Color where
  show Red = "Red"
  show Green = "Green"
  show Blue = "Blue"

instance showRegularOrAnti :: Show RegularOrAnti where
  show Regular = "Regular"
  show Anti = "Anti"

instance showQuark :: Show Quark where
  show = pretty

instance prettyPrintQuark :: PrettyPrint Quark where
  pretty (Quark { matterType: Anti, color: color, quarkType: quarkType }) =
    pretty Anti <> "-" <> pretty color <> " " <> pretty Anti <> "-" <> pretty quarkType
  pretty (Quark { matterType: Regular, color: color, quarkType: quarkType }) =
    pretty color <> " " <> pretty quarkType

instance prettyPrintQuarkType :: PrettyPrint QuarkType where
  pretty = show

instance prettyPrintColor :: PrettyPrint Color where
  pretty = show

instance prettyPrintRegularOrAnti :: PrettyPrint RegularOrAnti where
  pretty = show

makeQuark :: RegularOrAnti -> Color -> QuarkType -> Quark
makeQuark mt c qt = Quark { matterType: mt, color: c, quarkType: qt }
