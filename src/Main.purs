module Main where

import Prelude

{- import Control.Monad.Trans.Class (lift) -}

import Effect (Effect)

import Halogen.Aff as HAff
import Halogen.VDom.Driver (runUI)

import FourColors.Game as Game

{- Main Loop -}
main :: Effect Unit
main =
  HAff.runHalogenAff do
    body <- HAff.awaitBody
    runUI Game.component unit body
