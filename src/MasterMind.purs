module MasterMind where

import Prelude

import Data.Array
import Data.Maybe

import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Random (randomInt)

{-A generic MasterMind game interface

This should provide a generic engine for running a MasterMind style code breaking game, without assuming the length of the game or the type of code being played with.

For example, the original Mastermind uses four colored pegs and 8 - 12 guesses and 8 - 12 guesses. If someone wanted to instead use 5 letters and 6 guesses (Wordle), it should be possible to use this library to implement the mechanic.

    - *target* represents the type of data that the player should guess. Traditional example might be a FourColors type.
    - FeedBack represents the coded response to a single element of a guess.


TODO
    handleQuery
    make a game
-}

{- Data definitions -}

data FeedBack = Correct | Partial
derive instance eqFeedBack :: Eq FeedBack
derive instance ordFeedBack :: Ord FeedBack

instance showFeedBack :: Show FeedBack where 
    show Correct = "Correct"
    show Partial = "Partial"

{- When the player takes a turn, they should get feedback. -}
type Turn target =
    { guess :: target
    , feedback :: Array FeedBack
    }

{- Board provides the target the player is trying to guess, an array of turns already played. -}
type Board target =
    { target :: target
    , turns :: Array (Turn target)
    }

{- Arguments should take form `evalGuess target guess` so that a curried `evalGuess target` function is easy to produce`-}
class MasterMind target where
    generateTarget :: Effect target
    evalGuess :: target -> target -> Array FeedBack

new :: forall target. MasterMind target => target -> Board target
new target = { target : target, turns: mempty }

initialize :: forall target. MasterMind target => Effect (Board target)
initialize = do
    target <- generateTarget
    pure $ new target

takeTurn :: forall target. MasterMind target => target -> Board target -> Board target
takeTurn newGuess board = board { turns = snoc board.turns $ { guess: newGuess, feedback: evalGuess board.target newGuess }}


{- Helpers -}

randomChoice :: forall a. a -> Array a -> Effect a
randomChoice defaultChoice choices =     
  do    
    selected <- randomInt 0 (length choices)    
    case choices !! selected of    
      Just c -> pure c    
      Nothing -> pure defaultChoice
