module FourColors.Board where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (toLower)

import Effect.Class (class MonadEffect)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import Halogen.Svg.Elements as SVG
import Halogen.Svg.Attributes as SVGAttr

import Web.HTML.Common (ClassName(..))

import FourColors.Core
import MasterMind as MM

{- `board` component displays prevously taken turns, including feedback. It does not produce outpput, but a query to add & evaluate a new turn will return a boolean indicating whether the turn was a game-winning guess -}

type State
  = Maybe (MM.Board FourColors)

{- Randomly generating the target is an effect. Therefore the initial state has to be `Nothing` and the mkComponent step must trigger `Initialize`. -}
data Action
  = Initialize

data Query a
  = EvalTurn FourColors (Boolean -> a)
  | Restart a

component :: forall input output m. MonadEffect m => Number -> Number -> H.Component Query input output m
component pegSize fillRatio =
  H.mkComponent
    { initialState
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { initialize = Just Initialize
              , handleAction = handleAction
              , handleQuery = handleQuery
              }
    }
  where
  initialState :: input -> State
  initialState _ = Nothing

  handleAction :: Action -> H.HalogenM State Action () output m Unit
  handleAction Initialize = do
    b <- H.liftEffect MM.initialize
    H.modify_ \_ -> Just b

  handleQuery :: forall action a. Query a -> H.HalogenM State action () output m (Maybe a)
  handleQuery (EvalTurn guess reply) = do
    H.modify_ \state -> (MM.takeTurn guess) <$> state
    newState <- H.get
    pure $ (reply <<< (\b -> b.target == guess)) <$> newState

  handleQuery (Restart a) = do
    b <- H.liftEffect MM.initialize
    H.modify_ \_ -> Just b
    pure $ Just a

  render :: State -> H.ComponentHTML Action () m
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
      [ SVGAttr.height pegSize
      , SVGAttr.width pegSize
      ]
      [ SVG.circle
          [ SVGAttr.r (pegSize / 2.0 * fillRatio)
          , SVGAttr.cx (pegSize / 2.0)
          , SVGAttr.cy (pegSize / 2.0)
          , SVGAttr.classes [ ClassName $ show color ]
          ]
      ]

  renderFeedBack :: forall w i. MM.FeedBack -> HH.HTML w i
  renderFeedBack fb =
    SVG.svg
      [ SVGAttr.height pegSize
      , SVGAttr.width pegSize
      ]
      [ SVG.rect
          [ SVGAttr.width $ pegSize * fillRatio
          , SVGAttr.height $ pegSize * fillRatio
          , SVGAttr.classes [ ClassName <<< toLower <<< show $ fb ]
          ]
      ]
