module FourColors.Primary where

import Prelude
import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))
import FourColors.Game
import FourColors.Message as Msg
import FourColors.Chooser as Chooser
import FourColors.Board as Board

{- Four Color Game Controller in Halogen -}
type Slot
  = ( message :: forall query. H.Slot query Void Int
    , chooser :: forall query. H.Slot query Chooser.Output Int
    , board :: forall query. H.Slot query Void Int
    )

_message = Proxy :: Proxy "message"

_chooser = Proxy :: Proxy "chooser"

_board = Proxy :: Proxy "board"

type State
  = { game :: Game
    }

data Action
  = New
  | HandleChooser Chooser.Output

newGame :: forall m. Monad m => Game -> m State
newGame game = pure { game: game }

playerGuess :: Four Color -> State -> State
playerGuess x state = state { game = takeTurn x state.game }

mkChooserInput :: State -> Chooser.Input
mkChooserInput state = case status state.game of
  TurnsLeft _ -> true
  _ -> false

component :: forall query input output m. MonadEffect m => H.Component query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction
              , initialize = Just New
              }
    }
  where
  initialState :: input -> State
  initialState _ = { game: new $ Four Red Red Red Red }

  handleAction :: Action -> H.HalogenM State Action Slot output m Unit
  handleAction New = H.liftEffect initialize >>= newGame >>= H.put

  handleAction (HandleChooser output) = handleChooser output

  handleChooser :: Chooser.Output -> H.HalogenM State Action Slot output m Unit
  handleChooser (Chooser.TakeTurn x) = H.modify_ $ playerGuess x

  handleChooser Chooser.Restart = H.liftEffect initialize >>= newGame >>= H.put

  render :: State -> H.ComponentHTML Action Slot m
  render state =
    HH.div [ HP.id "game" ]
      [ HH.slot_ _message 0 Msg.component (status state.game)
      , HH.slot_ _board 1 Board.component (state.game.turns)
      , HH.slot _chooser 2 Chooser.component (mkChooserInput state) HandleChooser
      ]
