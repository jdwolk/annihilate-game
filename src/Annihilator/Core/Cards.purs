module Annihilator.Core.Cards where

import Prelude
import Data.Show (class Show)
import Data.Symbol (SProxy(..))
import Data.Lens (Prism', prism', preview, lens, only, is, view)
import Data.Lens.Record (prop)
import Data.Maybe

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
data Card = QuarkCard Quark | AnnihilateCard

derive instance eqQuarkType :: Eq QuarkType
derive instance eqColor :: Eq Color
derive instance eqRegularOrAnti :: Eq RegularOrAnti
derive instance eqQuark :: Eq Quark
derive instance eqCard :: Eq Card

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

instance showCard :: Show Card where
  show (QuarkCard quark) = "Quark(" <> show quark <> ")"
  show AnnihilateCard = "Annihlate"

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

quark :: RegularOrAnti -> Color -> QuarkType -> Quark
quark mt c qt = Quark { matterType: mt, color: c, quarkType: qt }

quarkCard :: RegularOrAnti -> Color -> QuarkType -> Card
quarkCard mt c qt = QuarkCard (quark mt c qt)

oppositeMatterType :: RegularOrAnti -> RegularOrAnti
oppositeMatterType Regular = Anti
oppositeMatterType _       = Regular

flipCard :: Card -> Card
flipCard AnnihilateCard = AnnihilateCard
flipCard (QuarkCard (Quark { matterType: mt, color: c, quarkType: qt })) =
  quarkCard (oppositeMatterType mt) c qt

-- lenses / prisms
_quarkCardFocus = prism' QuarkCard case _ of
  QuarkCard quark -> Just quark
  _               -> Nothing
_quark = lens (\(Quark q) -> q) (\_ -> Quark)
_matterType = lens _.matterType $ _ { matterType = _ }
_color = lens _.color $ _ { color = _ }

-- from the perspective of a QuarkCard
qcMatterType = preview (_quarkCardFocus <<< _quark <<< _matterType)
qcColor = preview (_quarkCardFocus <<< _quark <<< _color)
qcIsRegular qc = case qcMatterType qc of
  Just Regular -> true
  _            -> false
qcIsAnti qc = case qcMatterType qc of
  Just Anti -> true
  _         -> false
