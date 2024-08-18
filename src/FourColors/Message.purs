module FourColors.Message where

import Prelude
import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import FourColors.Game (PlayStatus(..))

{- Message Display Component -}
type State
  = PlayStatus

type Input
  = PlayStatus

data Action
  = Receive Input

component :: forall query output m. MonadEffect m => H.Component query Input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction
              , receive = Just <<< Receive
              }
    }
  where
  initialState :: Input -> State
  initialState = identity

  handleAction :: Action -> H.HalogenM State Action () output m Unit
  handleAction (Receive input) = H.put input

  render :: State -> H.ComponentHTML Action () m
  render state =
    HH.div
      [ HP.id "gameMessage" ]
      [ HH.text case state of
          Won -> "You won the game!"
          Lost -> "You lost the game."
          TurnsLeft x -> "Turns remaining: " <> show x
      ]
