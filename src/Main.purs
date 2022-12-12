module Main where

import Prelude

import Control.Applicative (when)
{- import Control.Monad.Trans.Class (lift) -}

import Data.Array as Array
import Data.List
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), uncurry, fst, snd)

import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Console (log)

import Halogen.Aff as HAff
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)

import Type.Proxy (Proxy(..))

import MasterMind as MM

{- Main Loop -}

main :: Effect Unit
main = HAff.runHalogenAff do
  body <- HAff.awaitBody
  runUI game unit body

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

getCorrect :: FourColors -> FourColors -> List MM.FeedBack
getCorrect target guess =
          map (\_ -> MM.Correct)
          <<< filter (uncurry (==))
          $ zip (fcList target) (fcList guess)

getPartial :: FourColors -> FourColors -> List MM.FeedBack
getPartial target guess = looper (map fst candidates) (map snd candidates) where
    candidates = filter (uncurry (/=)) $ zip (fcList target) (fcList guess)

    looper _ Nil = Nil
    looper unmatched (g:gs) =
        if g `elem` unmatched
            then MM.Partial : looper (delete g unmatched) gs
            else looper unmatched gs


instance masterMindFourColors :: MM.MasterMind FourColors where
    generateTarget = do
        one   <- MM.randomChoice Red colors
        two   <- MM.randomChoice Red colors
        three <- MM.randomChoice Red colors
        four  <- MM.randomChoice Red colors

        pure $ FourColors one two three four

    evalGuess target guess = Array.fromFoldable $ getCorrect target guess <> getPartial target guess

{- Four Colors Game in Halogen

    - html character codes
        - "black large circle" - &#11044; 
        - "black large square" - &#11035;
        - "black diamond centred" - &#11201;

    - parent
        - states
            - uninitialized (menu?)
            - playing int (turn count)
            - won
            - lost

        - actions
            - begin
            - take turn

    - choose component
        - state (record)
            - active :: boolean
            - pick :: Maybe Color
            - one :: Maybe Color
            - two :: Maybe Color
            - three :: Maybe Color
            - four :: Maybe Color

        - input (parent sends per render)
            - active boolean (should the component keep doing input? a won / lost game could simply not render a selector, but this is partly a practice)
        - internal actions
            - choose color
            - apply color to position
        - output (child alerts parent)
            - new guess
 fog869vat84type   
-}

type Slot =
    ( board :: H.Slot BoardQuery Void Int
    , chooser :: H.Slot ChooserQuery ChooserOutput Int
    )           

data GameState = GamePlay Int | GameOver Boolean
data GameAction = HandleChooser ChooserOutput

gameLength :: Int
gameLength = 10

game :: forall query input output m. MonadEffect m => H.Component query input output m
game =
    H.mkComponent
        { initialState
        , render
        , eval: H.mkEval $ H.defaultEval
            { handleAction = handleAction }
        }
    where

    initialState :: input -> GameState
    initialState _ = GamePlay 0

    handleAction :: GameAction -> H.HalogenM GameState GameAction Slot output m Unit
    handleAction (HandleChooser (TakeTurn fc)) = do
        ev <- H.request _board 0 (EvalTurn fc)
        case ev of
            Just turnWon -> H.modify_ $ handleTurn turnWon
            Nothing -> pure unit

    handleTurn :: Boolean -> GameState -> GameState
    handleTurn true _ = GameOver true
    handleTurn _ (GamePlay x) =
        if x < gameLength
            then GamePlay ( x + 1 )
            else GameOver false
    handleTurn _ over = over


    render :: GameState -> H.ComponentHTML GameAction Slot m
    render ( GamePlay turnsTaken ) =
        HH.div_
        [ HH.p_ [ HH.text $ "Turns taken " <> show turnsTaken ]
        , HH.slot_ _board 0 board Nothing
        , HH.slot_ _chooser 1 chooser { active: true }
        ]
    render ( GameOver won ) =
        HH.div_
        [ HH.p_ [ HH.text $ "You have " <> if won then "won" else "lost" <> " the game."
                , HH.slot_ _board 0 board Nothing
                , HH.slot_ _chooser 1 chooser { active: true }
                ]
        ]

{- Board Component -}

_board = Proxy :: Proxy "board"

type BoardState = Maybe (MM.Board FourColors)
    {- Randomly generating the target is an effect. Therefore the initial state has to be `Nothing` and the mkComponent step must trigger `InitializeBoard`. -}

data BoardAction = InitializeBoard
data BoardQuery a = EvalTurn FourColors (Boolean -> a)


board :: forall input output m. MonadEffect m => H.Component BoardQuery input output m
board = H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { initialize = Just InitializeBoard
        , handleAction = handleAction
        , handleQuery = handleQuery
        }
    }
    where

        initialState :: input -> BoardState
        initialState _ = Nothing

        handleAction :: BoardAction -> H.HalogenM BoardState BoardAction () output m Unit
        handleAction InitializeBoard = do
            b <- H.liftEffect MM.initialize
            H.modify_ \_ -> Just b

        handleQuery :: forall action a. BoardQuery a -> H.HalogenM BoardState action () output m (Maybe a)
        handleQuery (EvalTurn guess reply) = do
            H.modify_ \state -> (MM.takeTurn guess) <$> state
            newState <- H.get
            pure $ (reply <<< (\b -> b.target == guess)) <$> newState

        render :: forall m. BoardState -> H.ComponentHTML BoardAction () m
        render Nothing = HH.p_ [ HH.text "Loading Board" ]
        render (Just b) = HH.p_ [ HH.text $ show b.target ]


{- Selector Component -}

_chooser = Proxy :: Proxy "chooser"

type ChooserState =
    { active :: Boolean
    , pick   :: Maybe Color
    , one    :: Maybe Color
    , two    :: Maybe Color
    , three  :: Maybe Color
    , four   :: Maybe Color
    }

type ChooserInput = { active :: Boolean }

data ChooserAction = SetPick Color | SetOne Color | SetTwo Color | SetThree Color | SetFour Color | ChooserReceive ChooserInput | Submit FourColors

data ChooserOutput = TakeTurn FourColors
data ChooserQuery a = SetActive Boolean a


chooser :: forall query m. H.Component query ChooserInput ChooserOutput m
chooser = H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , receive = Just <<< ChooserReceive
        }
    } where

        initialState :: ChooserInput -> ChooserState
        initialState input =
            { active: input.active
            , pick: Nothing
            , one: Nothing
            , two: Nothing
            , three: Nothing
            , four: Nothing
            }

        render :: forall m. ChooserState -> H.ComponentHTML ChooserAction () m
        render cs = HH.p_ [ HH.text  "Your chooser goes here." ]

        handleAction :: ChooserAction -> H.HalogenM ChooserState ChooserAction () ChooserOutput m Unit
        handleAction (SetPick c) = H.modify_ \state -> state { pick = Just c }
        handleAction (SetOne c) = H.modify_ \state -> state { one = Just c }
        handleAction (SetTwo c) = H.modify_ \state -> state { two = Just c }
        handleAction (SetThree c) = H.modify_ \state -> state { three = Just c }
        handleAction (SetFour c) = H.modify_ \state -> state { four = Just c }
        handleAction (ChooserReceive input) = H.modify_ \state -> state { active = input.active }
        handleAction (Submit fc) = H.raise $ TakeTurn fc
