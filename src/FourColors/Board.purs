module FourColors.Board where

import Prelude
import Data.Array (fromFoldable)
import Data.Maybe (Maybe(..))
import Data.String (toLower)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Svg.Elements as SVG
import Halogen.Svg.Attributes as SVGAttr
import Web.HTML.Common (ClassName(..))
import FourColors.Game (Color, FeedBack, Four)

{- `board` component displays prevously taken turns, including feedback. -}
type Turn
  = { guess :: Four Color
    , feedback :: Array FeedBack
    }

type State
  = Array Turn

type Input
  = Array Turn

data Action
  = Receive Input

pegSize :: Number
pegSize = 30.0

component :: forall output query m. MonadEffect m => H.Component query Input output m
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
      [ HP.id "board" ]
      $ map renderTurn state

  renderTurn :: forall w i. Turn -> HH.HTML w i
  renderTurn turn =
    HH.div
      [ HP.classes [ ClassName "turn" ] ]
      [ HH.div [ HP.classes [ ClassName "guess" ] ]
          $ map renderPeg
          $ fromFoldable turn.guess
      , HH.div [ HP.classes [ ClassName "feedback" ] ]
          $ map renderFeedBack turn.feedback
      ]

  renderPeg :: forall w i. Color -> HH.HTML w i
  renderPeg color =
    SVG.svg
      [ SVGAttr.height pegSize
      , SVGAttr.width pegSize
      ]
      [ SVG.circle
          [ SVGAttr.r (pegSize / 2.0 - 2.0)
          , SVGAttr.cx (pegSize / 2.0)
          , SVGAttr.cy (pegSize / 2.0)
          , SVGAttr.classes [ ClassName $ show color ]
          ]
      ]

  renderFeedBack :: forall w i. FeedBack -> HH.HTML w i
  renderFeedBack fb =
    SVG.svg
      [ SVGAttr.height pegSize
      , SVGAttr.width pegSize
      ]
      [ SVG.rect
          [ SVGAttr.width $ pegSize - 5.0
          , SVGAttr.height $ pegSize - 5.0
          , SVGAttr.transform [ SVGAttr.Translate 2.5 2.5 ]
          , SVGAttr.classes [ ClassName <<< toLower <<< show $ fb ]
          ]
      ]
