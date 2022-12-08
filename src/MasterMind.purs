module MasterMind where

import Prelude

import Data.Array
import Data.Maybe

import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Random (randomInt)

import Halogen as H

{-A generic MasterMind game interface -}

{-
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
    winningGuess :: target -> target -> Boolean
    generateTarget :: Effect target
    evalGuess :: target -> target -> Array FeedBack

new :: forall target. MasterMind target => target -> Board target
new target = { target : target, turns: mempty }

takeTurn:: forall target. MasterMind target => Board target -> target -> Board target
takeTurn board newGuess = board { turns = snoc board.turns $ { guess: newGuess, feedback: sort $ evalGuess board.target newGuess }}


{- Helpers -}

randomChoice :: forall a. a -> Array a -> Effect a    
randomChoice defaultChoice choices =     
  do    
    selected <- randomInt 0 (length choices)    
    case choices !! selected of    
      Just c -> pure c    
      Nothing -> pure defaultChoice

{- Halogen -}

type State target = Board target

data Action target = Initialize | TakeTurn target

handleAction :: forall target query output m. MonadEffect m => MasterMind target => Action target -> H.HalogenM (State target) (Action target) query output m Unit
handleAction Initialize = do
       target :: target <- H.liftEffect generateTarget
       H.modify_ (\_ -> new target)
handleAction (TakeTurn turn) = H.modify_ \state -> takeTurn state turn
