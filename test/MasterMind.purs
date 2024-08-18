module Test.MasterMind where

import Prelude
import Test.Unit (suite, test, TestSuite)
import Test.Unit.Assert as Assert
import MasterMind

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
