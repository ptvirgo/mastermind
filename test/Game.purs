module Test.Game where

import Prelude
import Data.Array
import Test.QuickCheck
import Test.QuickCheck.Gen
import FourColors.Game

newGame :: forall m. Monad m => Four Color -> m Game
newGame target = pure <<< new $ target

propWonGame :: Gen Boolean
propWonGame = do
  game <- arbitrary >>= newGame
  count <- chooseInt 1 5
  turns <- listOf count $ suchThat arbitrary (_ /= game.target)
  let
    played = takeTurn game.target <<< foldr takeTurn game <<< fromFoldable $ turns
  pure $ status played == Won
