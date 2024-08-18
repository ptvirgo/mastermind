module FourColors.Chooser where

import Prelude
import Data.Maybe (Maybe(..), fromMaybe)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Svg.Elements as SVG
import Halogen.Svg.Attributes as SVGAttr
import Web.HTML.Common (ClassName(..))
import FourColors.Game (Color, Four(..), colors)

{- `chooser` Component allows the player to prepare and submit guesses.  Handles player color selections internally, but passes submitted guesses to the parent element via Output. -}

type State
  = { active :: Boolean
    , pick :: Maybe Color
    , one :: Maybe Color
    , two :: Maybe Color
    , three :: Maybe Color
    , four :: Maybe Color
    }

setActive :: Boolean -> State -> State
setActive active state = state { active = active }

setPick :: Color -> State -> State
setPick color state = state { pick = Just color }

setOne :: State -> State
setOne state = state { one = state.pick }

setTwo :: State -> State
setTwo state = state { two = state.pick }

setThree :: State -> State
setThree state = state { three = state.pick }

setFour :: State -> State
setFour state = state { four = state.pick }

type Input
  = Boolean

data Action
  = SetPick Color
  | SetOne
  | SetTwo
  | SetThree
  | SetFour
  | Receive Input
  | Submit (Four Color)
  | ClickRestart

data Output
  = TakeTurn (Four Color)
  | Restart

pegSize :: Number
pegSize = 60.0

swatchSize :: Number
swatchSize = 40.0

component :: forall query m. H.Component query Input Output m
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
  initialState input =
    { active: input
    , pick: Nothing
    , one: Nothing
    , two: Nothing
    , three: Nothing
    , four: Nothing
    }

  handleAction :: Action -> H.HalogenM State Action () Output m Unit
  handleAction (SetPick c) = H.modify_ $ setPick c

  handleAction SetOne = H.modify_ $ setOne

  handleAction SetTwo = H.modify_ $ setTwo

  handleAction SetThree = H.modify_ $ setThree

  handleAction SetFour = H.modify_ $ setFour

  handleAction (Receive input) = H.modify_ $ setActive input

  handleAction (Submit guess) = H.raise $ TakeTurn guess

  handleAction ClickRestart = H.raise $ Restart

  render :: State -> H.ComponentHTML Action () m
  render state =
    HH.div [ HP.classes (chooserClasses state.active), HP.id "chooser" ]
      [ HH.div_ $ [ renderPeg state.one SetOne, renderPeg state.two SetTwo, renderPeg state.three SetThree, renderPeg state.four SetFour ]
      , HH.div_ $ map (renderColor state.pick) colors
      , HH.div_
          [ if state.active then
              renderSubmit state
            else
              renderRestart
          ]
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
          [ SVGAttr.r (pegSize / 2.0 - 5.0)
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
          , SVGAttr.height $ swatchSize - 5.0
          , SVGAttr.width $ swatchSize - 5.0
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
  renderSubmit state = case Four <$> state.one <*> state.two <*> state.three <*> state.four of
    Just guess -> HH.button [ HE.onClick (\_ -> Submit guess) ] [ HH.text "Guess" ]
    Nothing -> HH.text ""

  renderRestart :: H.ComponentHTML Action () m
  renderRestart = HH.button [ HE.onClick (\_ -> ClickRestart) ] [ HH.text "Restart" ]
