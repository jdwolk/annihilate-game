module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)

import Annihilator.Core.Cards
import Annihilator.Core.Confinements

main :: Effect Unit
main = do
  redUp       <- pure $ quarkCard Regular Red Up
  blueStrange <- pure $ quarkCard Regular Blue Strange
  greenDown   <- pure $ quarkCard Regular Green Down
  antiRedDown <- pure $ quarkCard Anti Red Down
  annihilate  <- pure AnnihilateCard

  shouldBeValid <- pure $ isValidConfinement [redUp, antiRedDown]
  alsoValid     <- pure $ isValidConfinement [blueStrange, redUp, greenDown]
  invalid       <- pure $ isValidConfinement [blueStrange, greenDown, antiRedDown]
  alsoInvalid   <- pure $ isValidConfinement [annihilate, redUp, antiRedDown]

  log $ "Valid: " <> (show shouldBeValid)
  log $ "Valid: " <> (show alsoValid)
  log $ "Invalid: " <> (show $ not invalid)
  log $ "Invalid: " <> (show $ not alsoInvalid)
