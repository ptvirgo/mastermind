module FourColors.Core where

import Prelude

import Data.Array
import Data.Array.NonEmpty as NE

import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (elements)

import MasterMind (class MasterMind, defaultFeedBack)

{- Color represents one of the guessable colors -}
data Color
  = Red
  | Orange
  | Yellow
  | Green
  | Blue
  | Purple

derive instance eqColor :: Eq Color
derive instance ordColor :: Ord Color

instance showColor :: Show Color where
  show Red = "red"
  show Orange = "orange"
  show Yellow = "yellow"
  show Green = "green"
  show Blue = "blue"
  show Purple = "purple"

colors :: Array Color
colors = [ Red, Orange, Yellow, Green, Blue, Purple ]

instance arbColor :: Arbitrary Color where
    arbitrary = elements <<< foldr NE.(:) (NE.singleton Red) <<<  drop 1 $ colors

data FourColors
  = FourColors Color Color Color Color

derive instance eqFourColors :: Eq FourColors

instance showFourColors :: Show FourColors where
  show (FourColors one two three four) = "FourColors " <> show one <> " " <> show two <> " " <> show three <> " " <> show four

{- Convert a FourColor instance into a list, for cases when mapping or folding might be handy -}
fcArray :: FourColors -> Array Color
fcArray (FourColors a b c d) = [ a, b, c, d ]

instance masterMindFourColors :: MasterMind FourColors where
  evalGuess target guess = defaultFeedBack (fcArray target) (fcArray guess)

instance arbFourColors :: Arbitrary FourColors where
    arbitrary = FourColors <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
