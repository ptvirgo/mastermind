module Main where

import Prelude

import Control.Applicative (when)
{- import Control.Monad.Trans.Class (lift) -}

import Data.Array as Array
import Data.Char (fromCharCode)
import Data.String.CodeUnits (singleton)
import Data.List
import Data.Maybe (Maybe(..), fromMaybe)
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

import Halogen.Svg.Elements as SVG
import Halogen.Svg.Attributes as SVGAttr

import Type.Proxy (Proxy(..))
import Web.HTML.Common (ClassName(..))

import MasterMind as MM

{- Main Loop -}

main :: Effect Unit
main = HAff.runHalogenAff do
  body <- HAff.awaitBody
  runUI game unit body

{- Constants -}

svgLarge :: Number
svgLarge = 60.0

svgMedium :: Number
svgMedium = 40.0

svgSmall :: Number
svgSmall = 30.0

svgRatio :: Number
svgRatio = 0.9

{- Data Definitions -}

data Color = Red | Orange | Yellow | Green | Blue | Purple 

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

colorPeg :: String
colorPeg = " 0 "

colorSelector :: String
colorSelector = " A "

feedbackPeg :: String
feedbackPeg = " F "

{- Four Colors Game in Halogen
    - html character codes
        - "⬤ ⬛ ⯁
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
            { handleAction = handleAction
            }
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
        if x < (gameLength - 1)
            then GamePlay ( x + 1 )
            else GameOver false
    handleTurn _ over = over


    render :: GameState -> H.ComponentHTML GameAction Slot m
    render gameState =
        HH.div_
        [ HH.p_ [ HH.text $ gameMessage gameState ]
        , HH.slot_ _board 0 board Nothing
        , HH.slot _chooser 1 chooser { active: activeGame gameState } HandleChooser
        ]

    gameMessage :: GameState -> String
    gameMessage (GamePlay turns) = "Turns Taken: " <> show turns
    gameMessage (GameOver won) = "You have " <> if won then "won" else "lost" <> " the game."

    activeGame :: GameState -> Boolean
    activeGame (GamePlay _) = true
    activeGame _ = false

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

data ChooserAction = SetPick Color | SetOne | SetTwo | SetThree | SetFour | ChooserReceive ChooserInput | Submit FourColors

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


        handleAction :: ChooserAction -> H.HalogenM ChooserState ChooserAction () ChooserOutput m Unit
        handleAction (SetPick c) = H.modify_ \state -> state { pick = Just c }

        handleAction SetOne   = H.modify_ \state -> state { one   = state.pick }
        handleAction SetTwo   = H.modify_ \state -> state { two   = state.pick }
        handleAction SetThree = H.modify_ \state -> state { three = state.pick }
        handleAction SetFour  = H.modify_ \state -> state { four  = state.pick }

        handleAction (ChooserReceive input) = H.modify_ \state -> state { active = input.active }
        handleAction (Submit fc) = H.raise $ TakeTurn fc


        render :: ChooserState -> H.ComponentHTML ChooserAction () m
        render cs = HH.div [ HP.classes (chooserClasses cs.active) ]
            [ HH.div_ $ [ renderPeg cs.one SetOne, renderPeg cs.two SetTwo, renderPeg cs.three SetThree, renderPeg cs.four SetFour]
            , HH.div_ $ map (renderColor cs.pick) colors
            , HH.div_ [ renderSubmit cs ]
            ]

        chooserClasses :: Boolean -> Array ClassName
        chooserClasses active = [ ClassName "chooser", ClassName status ] where
            status = if active then "active" else "inactive"

        renderPeg :: Maybe Color -> ChooserAction -> H.ComponentHTML ChooserAction () m
        renderPeg mc action =
            SVG.svg
                [ SVGAttr.height svgLarge
                , SVGAttr.width svgLarge
                ]
                [ SVG.circle
                    [ SVGAttr.r (svgLarge / 2.0 * svgRatio)
                    , SVGAttr.cx (svgLarge / 2.0)
                    , SVGAttr.cy (svgLarge / 2.0)
                    , SVGAttr.classes $ pegClasses mc
                    , HE.onClick (\_ -> action)
                    ]
                ]

        pegClasses :: Maybe Color -> Array ClassName
        pegClasses mc = [ ClassName "peg", ClassName $ fromMaybe "unselected" ( show <$> mc ) ]

        {- renderColor :: Maybe Color -> Color -> H.ComponentHTML ChooserAction () m
        renderColor pick c = HH.span [ HP.classes $ colorClasses pick c, HE.onClick (\_ -> SetPick c ) ]
                                     [ HH.text colorSelector ]

        -}

        renderColor :: Maybe Color -> Color -> H.ComponentHTML ChooserAction () m
        renderColor pick c =
            SVG.svg
                [ SVGAttr.height svgMedium
                , SVGAttr.width svgMedium
                ]
                [ SVG.rect
                    [ SVGAttr.classes $ colorClasses pick c
                    , SVGAttr.height $ svgMedium * svgRatio - 5.0
                    , SVGAttr.width $ svgMedium * svgRatio - 5.0
                    , SVGAttr.x 2.5
                    , SVGAttr.y 2.5
                    , HE.onClick (\_ -> SetPick c  )
                    ]
                ]

        colorClasses :: Maybe Color -> Color -> Array ClassName
        colorClasses pick c = [ ClassName "color", ClassName $ show c, ClassName picked ] where
          picked = if Just c == pick then "selected" else "unselected"

        renderSubmit :: ChooserState -> H.ComponentHTML ChooserAction () m
        renderSubmit cs = case FourColors <$> cs.one <*> cs.two <*> cs.three <*> cs.four of
                Just fc -> renderSubmitButton cs.active fc
                Nothing -> HH.text ""

        renderSubmitButton :: Boolean -> FourColors -> H.ComponentHTML ChooserAction () m
        renderSubmitButton active fc =
            HH.button [ HP.disabled $ not active, HE.onClick (\_ -> Submit fc)]
                      [ HH.text "Guess" ]
