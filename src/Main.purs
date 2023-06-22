module Main where

import Prelude

{- import Control.Monad.Trans.Class (lift) -}

import Data.Maybe
import Effect (Effect)

import Halogen.Aff as HAff
import Halogen.VDom.Driver (runUI)

import Web.DOM.ParentNode (QuerySelector(..))

import FourColors.Game as Game

main :: Effect Unit
main = HAff.runHalogenAff do
    body <- HAff.awaitBody
    appElement <- HAff.selectElement $ QuerySelector "div#app"
    case appElement of
        Just something -> runUI Game.component unit something
        Nothing -> runUI Game.component unit body
