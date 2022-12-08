module Main where

import Prelude

import Data.List
import Data.Array as Array
import Data.Tuple (Tuple(..), uncurry, fst, snd)

import Effect (Effect)
import Effect.Console (log)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import MasterMind

{- Data Definitions -}

data Color = Red | Orange | Yellow | Green | Blue | Purple 

derive instance eqColor :: Eq Color
derive instance ordColor :: Ord Color

instance showColor :: Show Color where
    show Red = "Red"
    show Orange = "Orange"
    show Yellow = "Yellow"
    show Green = "Green"
    show Blue = "Blue"
    show Purple = "Purple"

colors :: Array Color
colors = [ Red, Orange, Yellow, Green, Blue, Purple ]

data FourColors = FourColors Color Color Color Color

derive instance eqFourColors :: Eq FourColors
instance showFourColors :: Show FourColors where
    show (FourColors one two three four) = "FourColors " <> show one <> " " <> show two <> " " <> show three <> " " <> show four

fcList :: FourColors -> List Color
fcList (FourColors a b c d) = a : b : c : d : Nil

getCorrect :: FourColors -> FourColors -> List FeedBack
getCorrect target guess =
          map (\_ -> Correct)
          <<< filter (uncurry (==))
          $ zip (fcList target) (fcList guess)

getPartial :: FourColors -> FourColors -> List FeedBack
getPartial target guess = looper (map fst candidates) (map snd candidates) where
    candidates = filter (uncurry (/=)) $ zip (fcList target) (fcList guess)

    looper _ Nil = Nil
    looper unmatched (g:gs) =
        if g `elem` unmatched
            then Partial : looper (delete g unmatched) gs
            else looper unmatched gs


instance masterMindFourColors :: MasterMind FourColors where
    winningGuess target guess = target == guess
    generateTarget = do
        one <- randomChoice Red colors
        two <- randomChoice Red colors
        three <- randomChoice Red colors
        four <- randomChoice Red colors
        pure $ FourColors one two three four

    evalGuess target guess = Array.fromFoldable $ getCorrect target guess <> getPartial target guess


main :: Effect Unit
main = do
  log "🍝"

