module Test.Game where

import Prelude
import Data.Array
import Test.QuickCheck (arbitrary)
import Test.QuickCheck.Gen (Gen, chooseInt, listOf, suchThat)
import FourColors.Game

newGame :: forall m. Monad m => Four Color -> m Game
newGame target = pure <<< new $ target

inProgress :: Int -> Gen Game
inProgress count = do
  game <- arbitrary >>= newGame
  turns <- listOf count $ suchThat arbitrary (_ /= game.target)
  pure <<< foldr takeTurn game <<< fromFoldable $ turns

propWonGame :: Gen Boolean
propWonGame = do
  count <- chooseInt 1 9
  game <- inProgress count
  let
    played = takeTurn game.target game
  pure $ status played == Won

propLostGame :: Gen Boolean
propLostGame = do
  game <- inProgress 10
  pure $ status game == Lost

propInProgress :: Gen Boolean
propInProgress = do
  count <- chooseInt 1 9
  game <- inProgress count
  case status game of
    TurnsLeft _ -> pure true
    _ -> pure false
