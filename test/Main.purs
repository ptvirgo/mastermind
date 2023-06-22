module Test.Main where

import Prelude
import Effect (Effect)

import Test.Unit (suite, test, TestSuite)
import Test.Unit.Main (runTest)
import Test.Unit.Assert as Assert
import Test.QuickCheck as QC

import MasterMind

import FourColors.Core

{-
testScale :: TestSuite
testScale =
  suite "fitSizeToScale" do
    test "fitSizeToScale handles increases" do
      Assert.equal
        ( Scale 2.0 2.0)
        ( fitSizeToScale ({ height : 200, width : 100 }) ({ height : 50, width : 50 } ))
      Assert.equal
        ( Scale 4.0 4.0)
        ( fitSizeToScale ({ height : 100, width : 200 }) ({ height : 25, width : 25 } ))
    test "fitSizeToScale handles descreases" do
      Assert.equal
        ( Scale 0.5 0.5 )
        ( fitSizeToScale ({ height : 50, width : 75 }) ({ height : 100, width : 100 } ))
      Assert.equal
        ( Scale 0.5 0.5 )
        ( fitSizeToScale ({ height : 100, width : 150 }) ({ height : 200, width : 200 }))
-}
testMasterMind :: TestSuite
testMasterMind =
  suite "MasterMind" do
    test "sanity" do
      Assert.assert "the world has gone mad" true
    test "Orderable FeedBack" do
      Assert.assert "Correct should be < Partial" $ Correct < Partial
    test "defaultCorrect single" do
      Assert.equal
        1
        $ defaultCorrect [ 1, 2, 3, 4 ]
            [ 4, 1, 3, 2 ]
    test "defaultCorrect pair" do
      Assert.equal
        2
        $ defaultCorrect [ 1, 2, 3, 4 ]
            [ 4, 2, 2, 4 ]
    test "defaultPartial tricky" do
      Assert.equal
        2
        $ defaultPartial [ 2, 2, 4, 4 ]
            [ 4, 2, 2, 1 ]
    test "defaultFeedBack summary" do
      Assert.equal
        [ Correct, Partial, Partial ]
        $ defaultFeedBack [ 2, 2, 4, 4 ]
            [ 4, 2, 2, 1 ]

testMain :: TestSuite
testMain =
  suite "FourColors" do
    test "evalGuess identical" do
      Assert.equal
        [ Correct, Correct, Correct, Correct ]
        $ evalGuess (FourColors Red Red Red Red) (FourColors Red Red Red Red)
    test "evalGuess partial" do
      Assert.equal
        [ Correct, Correct, Partial ] {- the mastermind board handles sorting to avoid leaking clues -}
        $ evalGuess (FourColors Red Yellow Green Blue) (FourColors Orange Yellow Red Blue)
    test "one correct no duplicates" do
      Assert.equal
        [ Correct ]
        $ evalGuess (FourColors Red Yellow Green Blue) (FourColors Green Orange Green Green)
    test "one partial, no duplicates" do
      Assert.equal
        [ Partial ]
        $ evalGuess (FourColors Red Yellow Green Blue) (FourColors Yellow Orange Purple Yellow)
    test "correct & partial with duplicates" do
      Assert.equal
        [ Correct, Partial ]
        $ evalGuess (FourColors Red Blue Red Blue) (FourColors Green Blue Blue Purple)

propFourEqual :: FourColors -> Boolean
propFourEqual fc = [ Correct, Correct, Correct, Correct ] == evalGuess fc fc

main :: Effect Unit
main = do
  runTest do
    testMasterMind
    testMain
  QC.quickCheck' 3 propFourEqual
