module Test.Main where

import Prelude

import Data.Array ((:))
import Data.Foldable as F
import Effect (Effect)

import Test.Unit (suite, test, TestSuite)
import Test.Unit.Main (runTest)
import Test.Unit.Assert as Assert
import Test.QuickCheck as QC

import MasterMind
import FourColors.Entities (Color(..), Four(..))

import Test.Game as Game

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


testFourColors :: TestSuite
testFourColors =
    suite "FourColors" do

        test "foldr works like a list" do
            Assert.equal
                [ 1, 2, 3, 4 ]
                $ F.foldr (:) [] (Four 1 2 3 4)

        test "foldl works like a list" do
            Assert.equal
                [ 4, 3, 2, 1 ]
                $ F.foldl (flip (:)) [] (Four 1 2 3 4)

        test "evalGuess identical" do
          Assert.equal
            [ Correct, Correct, Correct, Correct ]
            $ evalGuess (Four Red Red Red Red) (Four Red Red Red Red)

        test "evalGuess partial" do
          Assert.equal
            [ Correct, Correct, Partial ]
            $ evalGuess (Four Red Yellow Green Blue) (Four Orange Yellow Red Blue)

        test "one correct no duplicates" do
          Assert.equal
            [ Correct ]
            $ evalGuess (Four Red Yellow Green Blue) (Four Green Orange Green Green)

        test "one partial, no duplicates" do
          Assert.equal
            [ Partial ]
            $ evalGuess (Four Red Yellow Green Blue) (Four Yellow Orange Purple Yellow)

        test "correct & partial with duplicates" do
          Assert.equal
            [ Correct, Partial ]
            $ evalGuess (Four Red Blue Red Blue) (Four Green Blue Blue Purple)

propFourFirstFunctorLaw :: Four Int -> Boolean
propFourFirstFunctorLaw four = (identity <$> four) == four

propFourSecondFunctorLaw :: Four Int -> Boolean
propFourSecondFunctorLaw four =
    let f = (_ + 2)
        g = (_ * 3)
     in
        map (f <<< g) four == (map f <<< map g $ four)

propFourEqual :: (Four Color) -> Boolean
propFourEqual fc = [ Correct, Correct, Correct, Correct ] == evalGuess fc fc

main :: Effect Unit
main = do
    runTest do
        testMasterMind
        testFourColors
    QC.quickCheck' 5 propFourFirstFunctorLaw
    QC.quickCheck' 5 propFourSecondFunctorLaw
    QC.quickCheckGen' 5 Game.propWonGame

    {- QC.quickCheck' 3 propFourEqual -}
