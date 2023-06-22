module Main where

import Prelude

{- import Control.Monad.Trans.Class (lift) -}

import Data.Maybe (Maybe(..))

import Effect (Effect)
import Effect.Class (class MonadEffect)

import Halogen.Aff as HAff
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)

import Type.Proxy (Proxy(..))

import FourColors.Chooser as Chooser
import FourColors.Board as Board

{- Main Loop -}
main :: Effect Unit
main =
  HAff.runHalogenAff do
    body <- HAff.awaitBody
    runUI game unit body

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
{- Gameplay Int shows number of turns taken. GameOver Boolean shows end-game status. Eg. a game that has been lost would be GameOver false. After 2 guesses: Gameplay 2 -}
data GameState
  = GamePlay Int
  | GameOver Boolean

data GameAction
  = HandleChooser Chooser.Output
  | NewGame

game :: forall query input output m. MonadEffect m => H.Component query input output m
game =
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
  initialState :: input -> GameState
  initialState _ = GamePlay 0

  handleAction :: GameAction -> H.HalogenM GameState GameAction Slot output m Unit
  handleAction (HandleChooser (Chooser.TakeTurn fc)) = do
    ev <- H.request _board 0 (Board.EvalTurn fc)
    case ev of
      Just turnWon -> H.modify_ $ handleTurn turnWon
      Nothing -> pure unit

  handleAction NewGame = do
    H.tell _board 0 Board.Restart
    H.modify_ $ \_ -> GamePlay 0

  handleTurn :: Boolean -> GameState -> GameState
  handleTurn true _ = GameOver true

  handleTurn _ (GamePlay x) =
    if x < (gameLength - 1) then
      GamePlay (x + 1)
    else
      GameOver false

  handleTurn _ over = over

  render :: GameState -> H.ComponentHTML GameAction Slot m
  render gameState =
    HH.div [ HP.id "game" ]
      [ gameMessage gameState
      , HH.slot_ _board 0 (Board.component svgSmall svgRatio) Nothing
      , HH.slot _chooser 1 (Chooser.component svgLarge svgMedium svgRatio) { active: activeGame gameState } HandleChooser
      ]

  gameMessage :: GameState -> H.ComponentHTML GameAction Slot m
  gameMessage state =
    HH.div [ HP.id "gameMessage" ] case state of
      (GamePlay turns) -> [ HH.p_ [ HH.text $ "Turns taken: " <> show turns <> " of " <> show gameLength ] ]
      (GameOver won) ->
        [ HH.p_
            [ HH.text $ "You " <> (if won then "won" else "lost") <> " the game. "
            , HH.button [ HP.id "restart", HE.onClick (\_ -> NewGame) ] [ HH.text "Restart" ]
            ]
        ]

  activeGame :: GameState -> Boolean
  activeGame (GamePlay _) = true

  activeGame _ = false
