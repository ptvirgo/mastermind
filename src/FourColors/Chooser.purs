module FourColors.Chooser where

import Prelude

{- import Control.Monad.Trans.Class (lift) -}
import Data.Maybe (Maybe(..), fromMaybe)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import Halogen.Svg.Elements as SVG
import Halogen.Svg.Attributes as SVGAttr

import Web.HTML.Common (ClassName(..))

import FourColors.Core (Color, FourColors(..), colors)

{- `chooser` Component allows the player to prepare and submit guesses.  Handles player color selections internally, but passes submitted guesses to the parent element via Output. -}

type State
  = { active :: Boolean
    , pick :: Maybe Color
    , one :: Maybe Color
    , two :: Maybe Color
    , three :: Maybe Color
    , four :: Maybe Color
    }

type Input
  = { active :: Boolean }

data Action
  = SetPick Color
  | SetOne
  | SetTwo
  | SetThree
  | SetFour
  | Receive Input
  | Submit FourColors

data Output
  = TakeTurn FourColors

component :: forall query m. Number -> Number -> Number -> H.Component query Input Output m
component pegSize swatchSize fillRatio =
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
  initialState input =
    { active: input.active
    , pick: Nothing
    , one: Nothing
    , two: Nothing
    , three: Nothing
    , four: Nothing
    }

  handleAction :: Action -> H.HalogenM State Action () Output m Unit
  handleAction (SetPick c) = H.modify_ \state -> state { pick = Just c }

  handleAction SetOne = H.modify_ \state -> state { one = state.pick }
  handleAction SetTwo = H.modify_ \state -> state { two = state.pick }
  handleAction SetThree = H.modify_ \state -> state { three = state.pick }
  handleAction SetFour = H.modify_ \state -> state { four = state.pick }

  handleAction (Receive input) = H.modify_ \state -> state { active = input.active }

  handleAction (Submit fc) = H.raise $ TakeTurn fc

  render :: State -> H.ComponentHTML Action () m
  render cs =
    HH.div [ HP.classes (chooserClasses cs.active), HP.id "chooser" ]
      [ HH.div_ $ [ renderPeg cs.one SetOne, renderPeg cs.two SetTwo, renderPeg cs.three SetThree, renderPeg cs.four SetFour ]
      , HH.div_ $ map (renderColor cs.pick) colors
      , HH.div_ [ renderSubmit cs ]
      ]

  chooserClasses :: Boolean -> Array ClassName
  chooserClasses active = [ ClassName "chooser", ClassName status ]
    where
    status = if active then "active" else "inactive"

  renderPeg :: Maybe Color -> Action -> H.ComponentHTML Action () m
  renderPeg mc action =
    SVG.svg
      [ SVGAttr.height pegSize
      , SVGAttr.width pegSize
      ]
      [ SVG.circle
          [ SVGAttr.r (pegSize / 2.0 * fillRatio)
          , SVGAttr.cx (pegSize / 2.0)
          , SVGAttr.cy (pegSize / 2.0)
          , SVGAttr.classes $ pegClasses mc
          , HE.onClick (\_ -> action)
          ]
      ]

  pegClasses :: Maybe Color -> Array ClassName
  pegClasses mc = [ ClassName "peg", ClassName $ fromMaybe "unselected" (show <$> mc) ]

  renderColor :: Maybe Color -> Color -> H.ComponentHTML Action () m
  renderColor pick c =
    SVG.svg
      [ SVGAttr.height swatchSize
      , SVGAttr.width swatchSize
      ]
      [ SVG.rect
          [ SVGAttr.classes $ colorClasses pick c
          , SVGAttr.height $ swatchSize * fillRatio - 5.0
          , SVGAttr.width $ swatchSize * fillRatio - 5.0
          , SVGAttr.x 2.5
          , SVGAttr.y 2.5
          , HE.onClick (\_ -> SetPick c)
          ]
      ]

  colorClasses :: Maybe Color -> Color -> Array ClassName
  colorClasses pick c = [ ClassName "color", ClassName $ show c, ClassName picked ]
    where
    picked = if Just c == pick then "selected" else "unselected"

  renderSubmit :: State -> H.ComponentHTML Action () m
  renderSubmit cs = case FourColors <$> cs.one <*> cs.two <*> cs.three <*> cs.four of
    Just fc -> renderSubmitButton cs.active fc
    Nothing -> HH.text ""

  renderSubmitButton :: Boolean -> FourColors -> H.ComponentHTML Action () m
  renderSubmitButton active fc =
    HH.button [ HP.disabled $ not active, HE.onClick (\_ -> Submit fc) ]
      [ HH.text "Guess" ]
