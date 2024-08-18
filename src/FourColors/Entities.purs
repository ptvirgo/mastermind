module FourColors.Entities where

import Prelude
import Data.Array
import Data.Array.NonEmpty as NE
import Data.Functor
import Data.Foldable as Fold
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
  arbitrary = elements <<< NE.cons' Red <<< drop 1 $ colors

data Four x
  = Four x x x x

derive instance eqFourX :: Eq x => Eq (Four x)

instance showFourX :: Show x => Show (Four x) where
  show (Four one two three four) = "Four " <> show one <> " " <> show two <> " " <> show three <> " " <> show four

instance arbFourX :: Arbitrary x => Arbitrary (Four x) where
  arbitrary = Four <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance functorFour :: Functor Four where
  map f (Four a b c d) = Four (f a) (f b) (f c) (f d)

instance foldFour :: Fold.Foldable Four where
  foldr f z (Four a b c d) = f d >>> f c >>> f b >>> f a $ z
  foldl f z (Four a b c d) = flip f a >>> flip f b >>> flip f c >>> flip f d $ z
  foldMap f (Four a b c d) = f a <> f b <> f c <> f d

instance masterMindFC :: MasterMind (Four Color) where
  evalGuess target guess = defaultFeedBack t g
    where
    t = Fold.foldr (:) [] target

    g = Fold.foldr (:) [] guess
