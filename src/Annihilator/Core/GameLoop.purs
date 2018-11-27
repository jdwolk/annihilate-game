module Annihilator.Core.GameLoop where

import Prelude
import Data.Map as Map
import Data.Tuple
import Data.Show (class Show)
import Data.Array (elem)
import Partial.Unsafe (unsafePartial)
import Data.Maybe (Maybe(..), fromJust)
import Data.Either (Either(..))
import Data.Lens (lens, view, set)
import Annihilator.Core.Cards

data GamePhase = BeamPhase | ColliderPhase | RefreshPhase | GameOver
data GameState =
  GameState { currentPhase :: GamePhase }

type BeamDeck = Array Card

derive instance eqGamePhase :: Eq GamePhase
derive instance ordGamePhase :: Ord GamePhase
derive instance eqGameState :: Eq GameState

instance showGamePhase :: Show GamePhase where
  show BeamPhase = "BeamPhase"
  show ColliderPhase = "ColliderPhase"
  show RefreshPhase = "RefreshPhase"
  show GameOver = "GameOver"

instance showGameState :: Show GameState where
  show (GameState { currentPhase }) = "GameState { currentPhase: " <> show currentPhase <> " }"

validPhaseTransitions :: Map.Map GamePhase (Array GamePhase)
validPhaseTransitions =
  Map.fromFoldable [ Tuple BeamPhase [ColliderPhase, RefreshPhase, GameOver]
                   , Tuple ColliderPhase [GameOver, BeamPhase]
                   , Tuple GameOver [] :: Tuple GamePhase (Array GamePhase)
                   ]

transitionPhase :: GamePhase -> GamePhase -> Either String GamePhase
transitionPhase current next = result where
  result = if elem next nextPhases
           then Right next
           else Left $ "Cannot transition game phase from '" <> show current <> "' to '" <> show next
  nextPhases = unsafePartial $ fromJust $ Map.lookup current validPhaseTransitions

transition :: GamePhase -> GameState -> Either String GameState
transition nextPhase gs = result where
  result = (flip gsSetCurrentPhase) gs <$> validatedNextPhase
  validatedNextPhase = transitionPhase (gsCurrentPhase gs) nextPhase

-- lenses
_gameState = lens (\(GameState gs) -> gs) (\_ -> GameState)
_currentPhase = lens _.currentPhase $ _ { currentPhase = _ }

gsCurrentPhase = view (_gameState <<< _currentPhase)
gsSetCurrentPhase = set (_gameState <<< _currentPhase)
