module Annihilator.Core.GameLoop where

import Prelude
import Data.Map as Map
import Data.Tuple
import Data.Show (class Show)
import Annihilator.Core.Cards

data GamePhase = BeamPhase | ColliderPhase | RefreshPhase | GameOver

type BeamDeck = Array Card

derive instance eqGamePhase :: Eq GamePhase
derive instance ordGamePhase :: Ord GamePhase

instance showGamePhase :: Show GamePhase where
  show BeamPhase = "BeamPhase"
  show ColliderPhase = "ColliderPhase"
  show RefreshPhase = "RefreshPhase"
  show GameOver = "GameOver"

validPhaseTransitions :: Map.Map GamePhase (Array GamePhase)
validPhaseTransitions =
  Map.fromFoldable [ Tuple BeamPhase [ColliderPhase, RefreshPhase, GameOver]
                   , Tuple ColliderPhase [GameOver, BeamPhase]
                   , Tuple GameOver [] :: Tuple GamePhase (Array GamePhase)
                   ]

