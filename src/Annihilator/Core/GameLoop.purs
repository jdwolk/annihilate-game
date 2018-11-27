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

type BeamDeck = Array Card
type DiscardPile = Array Card
type AnnihilatedPile = Array Card
type BackgroundCards = Array Card
type DetectorCards = Array Card
type ColliderCards = Array Card

data GamePhase = NewGame | BeamPhase | ColliderPhase | RefreshPhase | GameOver
data GameState =
  GameState { currentPhase :: GamePhase
            , beamDeck :: BeamDeck
            , discard :: DiscardPile
            , annihilated :: AnnihilatedPile
            , background :: BackgroundCards
            , detector :: DetectorCards
            , collider :: ColliderCards
            }

derive instance eqGamePhase :: Eq GamePhase
derive instance ordGamePhase :: Ord GamePhase
derive instance eqGameState :: Eq GameState

instance showGamePhase :: Show GamePhase where
  show NewGame = "NewGame"
  show BeamPhase = "BeamPhase"
  show ColliderPhase = "ColliderPhase"
  show RefreshPhase = "RefreshPhase"
  show GameOver = "GameOver"

instance showGameState :: Show GameState where
  show (GameState { currentPhase, beamDeck, discard, annihilated, background, detector, collider }) =
    "GameState { currentPhase: " <> show currentPhase <> ", " <>
               " beamDeck: " <> show beamDeck <> ", " <>
               " discard: " <> show discard <> ", " <>
               " annihilated: " <> show annihilated <> ", " <>
               " background: " <> show background <> ", " <>
               " detector: " <> show detector <> ", " <>
               " collider: " <> show collider <> ", " <>
               " }"

newGameState :: BeamDeck -> GameState
newGameState beamDeck =
  GameState { currentPhase: NewGame
            , beamDeck
            , discard: []
            , annihilated: []
            , background: []
            , detector: []
            , collider: []
            }

validPhaseTransitions :: Map.Map GamePhase (Array GamePhase)
validPhaseTransitions =
  Map.fromFoldable [ Tuple NewGame [BeamPhase]
                   , Tuple BeamPhase [ColliderPhase, RefreshPhase, GameOver]
                   , Tuple ColliderPhase [GameOver, BeamPhase]
                   , Tuple GameOver [] :: Tuple GamePhase (Array GamePhase)
                   ]

transitionPhase :: GamePhase -> GamePhase -> Either String GamePhase
transitionPhase current next = result where
  result = if elem next nextPhases
           then Right next
           else Left $ "Cannot transition game phase from '" <> show current <> "' to '" <> show next
  nextPhases = unsafePartial $ fromJust $ Map.lookup current validPhaseTransitions

gsTransitionPhase :: GamePhase -> GameState -> Either String GameState
gsTransitionPhase nextPhase gs = result where
  result = (flip gsSetCurrentPhase) gs <$> validatedNextPhase
  validatedNextPhase = transitionPhase (gsCurrentPhase gs) nextPhase

-- lenses
_gameState = lens (\(GameState gs) -> gs) (\_ -> GameState)
_currentPhase = lens _.currentPhase $ _ { currentPhase = _ }
_discard = lens _.discard $ _ { discard = _ }
_beamDeck = lens _.beamDeck $ _ { beamDeck = _ }
_annihilated = lens _.annihilated $ _ { annihilated = _ }
_background = lens _.background $ _ { background = _ }
_detector = lens _.detector $ _ { detector = _ }
_collider = lens _.collider $ _ { collider = _ }

gsCurrentPhase = view (_gameState <<< _currentPhase)
gsDiscard = view (_gameState <<< _discard)
gsBeamDeck = view (_gameState <<< _beamDeck)
gsAnnihilated = view (_gameState <<< _annihilated)
gsBackground = view (_gameState <<< _background)
gsDetector = view (_gameState <<< _detector)
gsCollider = view (_gameState <<< _collider)

gsSetCurrentPhase = set (_gameState <<< _currentPhase)
gsSetDiscard = set (_gameState <<< _discard)
gsSetBeamDeck = set (_gameState <<< _beamDeck)
gsSetAnnihilated = set (_gameState <<< _annihilated)
gsSetBackground = set (_gameState <<< _background)
gsSetDetector = set (_gameState <<< _detector)
gsSetCollider = set (_gameState <<< _collider)
