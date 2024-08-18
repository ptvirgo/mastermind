module Test.Entities where

import Prelude
import Data.Array ((:))
import Data.Foldable as F
import Test.Unit (suite, test, TestSuite)
import Test.Unit.Assert as Assert
import MasterMind (FeedBack(..), evalGuess)
import FourColors.Entities (Color(..), Four(..))

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
  let
    f = (_ + 2)

    g = (_ * 3)
  in
    map (f <<< g) four == (map f <<< map g $ four)

propFourEqual :: (Four Color) -> Boolean
propFourEqual fc = [ Correct, Correct, Correct, Correct ] == evalGuess fc fc
