module Test.Main where

import Prelude
import Effect (Effect)
import Test.Unit.Main (runTest)
import Test.QuickCheck as QC
import Test.Entities as Entities
import Test.MasterMind as MM
import Test.Game as Game

main :: Effect Unit
main = do
  runTest do
    MM.testMasterMind
    Entities.testFourColors
  QC.quickCheck' 5 Entities.propFourFirstFunctorLaw
  QC.quickCheck' 5 Entities.propFourSecondFunctorLaw
  QC.quickCheckGen' 5 Game.propWonGame
  QC.quickCheckGen' 5 Game.propLostGame
  QC.quickCheckGen' 5 Game.propInProgress
