module FourColors.Game where

import Prelude

{- import Control.Monad.Trans.Class (lift) -}

import Data.Maybe (Maybe(..))

import Effect.Class (class MonadEffect)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import Type.Proxy (Proxy(..))

import FourColors.Chooser as Chooser
import FourColors.Board as Board

{- Constants -}
{- svgLarge ... svgSmall are pixel sizes used by the generated svg visual elements. -}
svgLarge :: Number
svgLarge = 60.0

svgMedium :: Number
svgMedium = 40.0

svgSmall :: Number
svgSmall = 30.0

svgRatio :: Number
svgRatio = 0.9

{- gameLength is how many guesses the player is allowed to make before the game ends -}
gameLength :: Int
gameLength = 10

{- Four Colors Game in Halogen -}
type Slot
  = ( board :: H.Slot Board.Query Void Int
    , chooser :: forall query. H.Slot query Chooser.Output Int
    )
_board = Proxy :: Proxy "board"
_chooser = Proxy :: Proxy "chooser"

{- `game` is the primary (parent) component.  It tracks turns, handles inter-modal coordination, and provides in-progress / win / lose states -}
{- Play Int shows number of turns taken. Over Boolean shows end-game status. Eg. a game that has been lost would be Over false. After 2 guesses: Play 2 -}

data State
  = Play Int
  | Over Boolean

data Action
  = HandleChooser Chooser.Output
  | New

component :: forall query input output m. MonadEffect m => H.Component query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction
              }
    }
  where
  initialState :: input -> State
  initialState _ = Play 0

  handleAction :: Action -> H.HalogenM State Action Slot output m Unit
  handleAction (HandleChooser (Chooser.TakeTurn fc)) = do
    ev <- H.request _board 0 (Board.EvalTurn fc)
    case ev of
      Just turnWon -> H.modify_ $ handleTurn turnWon
      Nothing -> pure unit

  handleAction New = do
    H.tell _board 0 Board.Restart
    H.modify_ $ \_ -> Play 0

  handleTurn :: Boolean -> State -> State
  handleTurn true _ = Over true

  handleTurn _ (Play x) =
    if x < (gameLength - 1) then
      Play (x + 1)
    else
      Over false

  handleTurn _ over = over

  render :: State -> H.ComponentHTML Action Slot m
  render gameState =
    HH.div [ HP.id "game" ]
      [ gameMessage gameState
      , HH.slot_ _board 0 (Board.component svgSmall svgRatio) Nothing
      , HH.slot _chooser 1 (Chooser.component svgLarge svgMedium svgRatio) { active: activeGame gameState } HandleChooser
      ]

  gameMessage :: State -> H.ComponentHTML Action Slot m
  gameMessage state =
    HH.div [ HP.id "gameMessage" ] case state of
      (Play turns) -> [ HH.p_ [ HH.text $ "Turns taken: " <> show turns <> " of " <> show gameLength ] ]
      (Over won) ->
        [ HH.p_
            [ HH.text $ "You " <> (if won then "won" else "lost") <> " the game. "
            , HH.button [ HP.id "restart", HE.onClick (\_ -> New) ] [ HH.text "Restart" ]
            ]
        ]

  activeGame :: State -> Boolean
  activeGame (Play _) = true

  activeGame _ = false

