module Main where

import Prelude

{- import Control.Monad.Trans.Class (lift) -}

import Data.String (toLower)
import Data.Maybe (Maybe(..))

import Effect (Effect)
import Effect.Class (class MonadEffect)

import Halogen.Aff as HAff
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Halogen.Svg.Elements as SVG
import Halogen.Svg.Attributes as SVGAttr

import Type.Proxy (Proxy(..))
import Web.HTML.Common (ClassName(..))

import FourColors.Core (Color, FourColors, fcArray)
import FourColors.Chooser as Chooser

import MasterMind as MM

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
  = ( board :: H.Slot BoardQuery Void Int
    , chooser :: forall query. H.Slot query Chooser.Output Int
    )
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
    ev <- H.request _board 0 (EvalTurn fc)
    case ev of
      Just turnWon -> H.modify_ $ handleTurn turnWon
      Nothing -> pure unit

  handleAction NewGame = do
    H.tell _board 0 Restart
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
      , HH.slot_ _board 0 board Nothing
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

{- `board` component displays prevously taken turns, including feedback. It does not produce outpput, but a query to add & evaluate a new turn will return a boolean indicating whether the turn was a game-winning guess -}
_board = Proxy :: Proxy "board"

type BoardState
  = Maybe (MM.Board FourColors)

{- Randomly generating the target is an effect. Therefore the initial state has to be `Nothing` and the mkComponent step must trigger `InitializeBoard`. -}
data BoardAction
  = InitializeBoard

data BoardQuery a
  = EvalTurn FourColors (Boolean -> a)
  | Restart a

board :: forall input output m. MonadEffect m => H.Component BoardQuery input output m
board =
  H.mkComponent
    { initialState
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { initialize = Just InitializeBoard
              , handleAction = handleAction
              , handleQuery = handleQuery
              }
    }
  where
  initialState :: input -> BoardState
  initialState _ = Nothing

  handleAction :: BoardAction -> H.HalogenM BoardState BoardAction () output m Unit
  handleAction InitializeBoard = do
    b <- H.liftEffect MM.initialize
    H.modify_ \_ -> Just b

  handleQuery :: forall action a. BoardQuery a -> H.HalogenM BoardState action () output m (Maybe a)
  handleQuery (EvalTurn guess reply) = do
    H.modify_ \state -> (MM.takeTurn guess) <$> state
    newState <- H.get
    pure $ (reply <<< (\b -> b.target == guess)) <$> newState

  handleQuery (Restart a) = do
    b <- H.liftEffect MM.initialize
    H.modify_ \_ -> Just b
    pure $ Just a

  render :: BoardState -> H.ComponentHTML BoardAction () m
  render Nothing = HH.p_ [ HH.text "Loading Board" ]

  render (Just b) =
    HH.div
      [ HP.id "board" ]
      $ map renderTurn b.turns

  renderTurn :: forall w i. MM.Turn FourColors -> HH.HTML w i
  renderTurn t =
    HH.div
      [ HP.classes [ ClassName "turn" ] ]
      [ HH.div [ HP.classes [ ClassName "guess" ] ]
          $ map renderPeg (fcArray t.guess)
      , HH.div [ HP.classes [ ClassName "feedback" ] ]
          $ map renderFeedBack t.feedback
      ]

  renderPeg :: forall w i. Color -> HH.HTML w i
  renderPeg color =
    SVG.svg
      [ SVGAttr.height svgSmall
      , SVGAttr.width svgSmall
      ]
      [ SVG.circle
          [ SVGAttr.r (svgSmall / 2.0 * svgRatio)
          , SVGAttr.cx (svgSmall / 2.0)
          , SVGAttr.cy (svgSmall / 2.0)
          , SVGAttr.classes [ ClassName $ show color ]
          ]
      ]

  renderFeedBack :: forall w i. MM.FeedBack -> HH.HTML w i
  renderFeedBack fb =
    SVG.svg
      [ SVGAttr.height svgSmall
      , SVGAttr.width svgSmall
      ]
      [ SVG.rect
          [ SVGAttr.width $ svgSmall * svgRatio
          , SVGAttr.height $ svgSmall * svgRatio
          , SVGAttr.classes [ ClassName <<< toLower <<< show $ fb ]
          ]
      ]
