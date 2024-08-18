module FourColors.Game
  ( module ReExport
  , Game(..)
  , PlayStatus(..)
  , status
  ) where

import Prelude
import Data.Array (last, length)
import Data.Maybe (Maybe(..))
import FourColors.Entities (Color, Four)
import MasterMind (Board)
import MasterMind (FeedBack(..), new, initialize, takeTurn) as ReExport
import FourColors.Entities (Color(..), Four(..), colors) as ReExport

{- Boundary notes from Clean Architecture:

Per Clean Architecture, the use case layer should provide a predictable interface, and also a boundary, between the business rules and the controllers that call them. Controllers should not expect knowledge of, or access to, underlying entities, internal logic, etc. SRP in that work clearly states that having the underlying objects / data types cross this boundary is a recipe for dependency problems, and recommends that only very simple data structures be allowed back and forth.

It's less obvious how to interpret SRP & "only very simple data structures" when you've got algebraic data types, and translating a structure like "Four Color" into a string in the context of Purescript would defeat the purpose of strong typing.

To maintain both the boundary and the typing: Re-exports from here are allowed, but assume that (within the context of this FourColors module) importing from MasterMind or FourColors.Entities elsewhere is incorrect.  This allows for the possibility that if an SRP style dependency problem arisise, the re-export can be replaced with an appropriate custom data type.
-}
maxTurns :: Int
maxTurns = 10

type Game
  = Board (Four Color)

data PlayStatus
  = Won
  | Lost
  | TurnsLeft Int

derive instance eqPlayStatus :: Eq PlayStatus

instance showPlayStatus :: Show PlayStatus where
  show Won = "Won"
  show Lost = "Lost"
  show (TurnsLeft x) = "TurnsLeft " <> show x

{- Use case interface -}
status :: Game -> PlayStatus
status game
  | turnsLeft game >= 0 && won game = Won
  | turnsLeft game < 1 = Lost
  | otherwise = TurnsLeft $ turnsLeft game

won :: Game -> Boolean
won game = Just game.target == (_.guess <$> last game.turns)

turnsLeft :: Game -> Int
turnsLeft game = maxTurns - length game.turns
