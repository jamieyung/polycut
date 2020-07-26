module HueDeltaExplanation
    ( Action
    , Output(..)
    , Query(..)
    , Input
    , State
    , Slot
    , component
    ) where

import Prelude

import Data.Array as A
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen (ClassName(..))
import Halogen (Component, ComponentHTML, HalogenM, defaultEval, get, mkComponent, mkEval, modify) as H
import Halogen.Data.Slot (Slot) as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Slider (format_as_percentage)
import Slider as Slider
import Util (Hsl_to_hex, Hsl_to_rgb, hsl_to_hex, hsl_to_rgb, n_decimal_places_for_hue_and_lightness, render_range)

--------------------------------------------------------------------------------
-- Action, Query ---------------------------------------------------------------
--------------------------------------------------------------------------------

data Action
    = Initialize
    | Set_LR Number Number
    | Set_start Number

data Output

data Query a

--------------------------------------------------------------------------------
-- Input, State, Aliases, Component def ----------------------------------------
--------------------------------------------------------------------------------

type Input = Unit

type State =
    { l :: Number
    , r :: Number
    , start :: Number
    }

type M = H.HalogenM State Action ChildSlots Output

type Html m = H.ComponentHTML Action ChildSlots m

component :: forall m. MonadAff m => H.Component HH.HTML Query Input Output m
component = H.mkComponent
    { initialState: const { l: -0.1, r: 0.1, start: 0.75 }
    , render
    , eval: H.mkEval $ H.defaultEval
        { initialize = Just Initialize
        , handleAction = handle_action
        }
    }

--------------------------------------------------------------------------------
-- Slots -----------------------------------------------------------------------
--------------------------------------------------------------------------------

type ChildSlots =
    ( slider :: Slider.Slot String
    )

_slider = SProxy :: SProxy "slider"

type Slot = H.Slot Query Output

--------------------------------------------------------------------------------
-- Render ----------------------------------------------------------------------
--------------------------------------------------------------------------------

render :: forall m. MonadAff m => State -> Html m
render st = HH.div
    [ HP.class_ $ ClassName "width100" ]
    [ HH.div [ HP.class_ $ ClassName "flex_center_h" ] [ HH.div [ HP.id_ "hue_delta_explanation_canvas_container" ] [] ]

    , HH.b_ [ HH.text $ "Colour change " <> render_range n_decimal_places_for_hue_and_lightness { min: st.l, max: st.r } ]
    , HH.slot _slider "hue_delta_explanation_colour_change_slider" Slider.component
        { id: "hue_delta_explanation_colour_change_slider"
        , start: [st.l, st.r]
        , show_pips: true
        , range:
            [ { k: "min", v: -0.5, step: 0.001 }
            , { k: "50%", v: 0.0, step: 0.001 }
            , { k: "max", v: 0.5, step: 0.001 }
            ]
        , format: format_as_percentage n_decimal_places_for_hue_and_lightness
        } case _ of
            Slider.Slider_updated arr -> do
                l <- A.index arr 0
                r <- A.index arr 1
                pure $ Set_LR l r

    , HH.b_ [ HH.text $ "Original polygon colour" ]
    , HH.slot _slider "hue_delta_explanation_start_slider" Slider.component
        { id: "hue_delta_explanation_start_slider"
        , start: [st.start]
        , show_pips: false
        , range:
            [ { k: "min", v: 0.0, step: 0.01 }
            , { k: "max", v: 1.0, step: 0.01 }
            ]
        , format: format_as_percentage $ const 0
        } case _ of
            Slider.Slider_updated arr -> do
                n <- A.index arr 0
                pure $ Set_start n
    , HH.p_
        [ HH.text "When a polygon is cut in two, the colour of each sub-polygon is randomly selected from a range controlled by the "
        , HH.b_ [ HH.text "Colour change" ]
        , HH.text " parameter. This is visualized above."
        ]
    , HH.p_ [ HH.text "The little coloured notch on the outside of the ring indicates the colour of the original polygon." ]
    , HH.p_ [ HH.text "The coloured section inside the ring indicates the range of possible colours for the sub-polygons." ]
    , HH.p_ [ HH.text "Play around with the sliders to get a feel for it." ]
    , HH.p_
        [ HH.b_ [ HH.text "Tip 1: " ]
        , HH.text "If you want the colour change to be less pronounced, bring the slider range closer to "
        , HH.b_ [ HH.text "0%" ]
        , HH.text "."
        ]
    , HH.p_
        [ HH.b_ [ HH.text "Tip 2: " ]
        , HH.text "Try making the slider range only positive or only negative!"
        ]
    ]

--------------------------------------------------------------------------------
-- Eval ------------------------------------------------------------------------
--------------------------------------------------------------------------------

handle_action :: forall m. MonadEffect m => Action -> M m Unit
handle_action = case _ of
    Initialize -> do
        liftEffect $ init { hsl_to_rgb, hsl_to_hex }
        H.get >>= \st -> liftEffect $ redraw st
    Set_LR l r -> do
        st <- H.modify _ { l = l, r = r }
        liftEffect $ redraw st
    Set_start s -> do
        st <- H.modify _ { start = s }
        liftEffect $ redraw st

foreign import init :: { hsl_to_rgb :: Hsl_to_rgb, hsl_to_hex :: Hsl_to_hex } -> Effect Unit
foreign import redraw :: State -> Effect Unit
