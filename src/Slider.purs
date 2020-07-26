module Slider
    ( Action
    , Output(..)
    , Query(..)
    , Input
    , State
    , Slot
    , component
    , format_as_percentage
    ) where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number as Number
import Data.String as String
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Global (toFixed)
import Halogen (Component, ComponentHTML, HalogenM, defaultEval, get, mkComponent, mkEval, raise, subscribe) as H
import Halogen.Data.Slot (Slot) as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource (eventListenerEventSource)
import Web.Event.Event (Event, EventType(..))
import Web.Event.EventTarget (EventTarget)

--------------------------------------------------------------------------------
-- Action, Query ---------------------------------------------------------------
--------------------------------------------------------------------------------

data Action
    = Initialize
    | Handle_slider_updated (Array Number)

data Output
    = Slider_updated (Array Number)

data Query a

--------------------------------------------------------------------------------
-- Input, State, Aliases, Component def ----------------------------------------
--------------------------------------------------------------------------------

type Input = State

type State =
    { id :: String
    , start :: Array Number
    , range :: Array { k :: String, v :: Number, step :: Number }
    , show_pips :: Boolean
    , format ::
        { to :: Number -> String
        , from :: String -> Number
        }
    }

type M = H.HalogenM State Action ChildSlots Output

type Html m = H.ComponentHTML Action ChildSlots m

component :: forall m. MonadAff m => H.Component HH.HTML Query Input Output m
component = H.mkComponent
    { initialState: identity
    , render
    , eval: H.mkEval $ H.defaultEval
        { initialize = Just Initialize
        , handleAction = handle_action
        , handleQuery = handle_query
        }
    }

format_as_percentage :: (Number -> Int) -> { from :: String -> Number, to :: Number -> String }
format_as_percentage n_decimal_places =
    { to: \n ->
        let pct = n * 100.0
        in (fromMaybe "0" $ toFixed (n_decimal_places pct) pct) <> "%"
    , from: \s -> fromMaybe 0.5 do
        let s' = String.take (String.length s - 1) s
        n <- Number.fromString s'
        pure $ n / 100.0
    }

--------------------------------------------------------------------------------
-- Slots -----------------------------------------------------------------------
--------------------------------------------------------------------------------

type ChildSlots = ()

type Slot = H.Slot Query Output

--------------------------------------------------------------------------------
-- Render ----------------------------------------------------------------------
--------------------------------------------------------------------------------

render :: forall m. State -> Html m
render st = HH.div [ HP.id_ st.id ] []

--------------------------------------------------------------------------------
-- Eval ------------------------------------------------------------------------
--------------------------------------------------------------------------------

handle_action :: forall m. MonadAff m => Action -> M m Unit
handle_action = case _ of
    Initialize -> do
        st <- H.get
        slider <- liftEffect $ init_no_ui_slider st
        let et = to_event_target slider
            f = slider_update_listener \arr -> Just $ Handle_slider_updated arr
        void $ H.subscribe $ eventListenerEventSource (EventType "slider_update") et f

    Handle_slider_updated arr -> H.raise $ Slider_updated arr

handle_query :: forall m a. Query a -> M m (Maybe a)
handle_query = case _ of
    _ -> pure Nothing

foreign import data No_ui_slider :: Type
foreign import init_no_ui_slider :: State -> Effect No_ui_slider
foreign import to_event_target :: No_ui_slider -> EventTarget
foreign import slider_update_listener :: (Array Number -> Maybe Action) -> (Event -> Maybe Action)
