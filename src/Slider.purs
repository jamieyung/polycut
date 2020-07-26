module Slider
    ( Action
    , Output(..)
    , Query(..)
    , Input
    , State
    , Shared
    , Slot
    , component
    , format_as_percentage
    , No_ui_slider
    ) where

import Prelude

import Data.Function.Uncurried (Fn2, runFn2)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number as Number
import Data.String as String
import Data.Symbol (SProxy(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Global (toFixed)
import Halogen (Component, ComponentHTML, HalogenM, defaultEval, get, mkComponent, mkEval, modify_, raise, subscribe) as H
import Halogen.Data.Slot (Slot) as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource (eventListenerEventSource)
import Record as Record
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
    = Set_values (Array Number) a

--------------------------------------------------------------------------------
-- Input, State, Aliases, Component def ----------------------------------------
--------------------------------------------------------------------------------

type Input = Record Shared

type State =
    { m_slider :: Maybe No_ui_slider
    | Shared
    }

type Shared =
    ( id :: String
    , start :: Array Number
    , range :: Array { k :: String, v :: Number, step :: Number }
    , show_pips :: Boolean
    , format ::
        { to :: Number -> String
        , from :: String -> Number
        }
    )

type M = H.HalogenM State Action ChildSlots Output

type Html m = H.ComponentHTML Action ChildSlots m

component :: forall m. MonadAff m => H.Component HH.HTML Query Input Output m
component = H.mkComponent
    { initialState: Record.insert (SProxy :: SProxy "m_slider") Nothing
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
        H.modify_ _ { m_slider = Just slider }

    Handle_slider_updated arr -> H.raise $ Slider_updated arr

handle_query :: forall m a. MonadEffect m => Query a -> M m (Maybe a)
handle_query = case _ of
    Set_values values a -> H.get >>= \st -> case st.m_slider of
        Nothing -> pure Nothing
        Just slider -> do
            liftEffect $ runFn2 set_values slider values
            pure $ Just a

foreign import data No_ui_slider :: Type
foreign import init_no_ui_slider :: State -> Effect No_ui_slider
foreign import to_event_target :: No_ui_slider -> EventTarget
foreign import set_values :: Fn2 No_ui_slider (Array Number) (Effect Unit)
foreign import slider_update_listener :: (Array Number -> Maybe Action) -> (Event -> Maybe Action)
