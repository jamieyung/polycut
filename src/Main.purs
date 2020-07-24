module Main where

import Prelude

import DOM.HTML.Indexed.InputType (InputType(..))
import DOM.HTML.Indexed.StepValue (StepValue(..))
import Data.Const (Const)
import Data.Foldable (traverse_)
import Data.Function.Uncurried (Fn2, runFn2)
import Data.Int as Int
import Data.Lens (Lens')
import Data.Lens as Lens
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Number as Number
import Data.Symbol (SProxy(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource (eventListenerEventSource)
import Halogen.VDom.Driver (runUI)
import Record as Record
import Web.DOM.ParentNode (QuerySelector(..))
import Web.Event.EventTarget (EventTarget)
import Web.TouchEvent.EventTypes (touchcancel, touchend, touchmove, touchstart)
import Web.TouchEvent.Touch as T
import Web.TouchEvent.TouchEvent as TE
import Web.TouchEvent.TouchList as TL
import Web.UIEvent.MouseEvent as ME
import Web.UIEvent.MouseEvent.EventTypes (mousedown, mousemove, mouseup)

-------------------------------------------------------------------------------
-- TODOS ----------------------------------------------------------------------
-------------------------------------------------------------------------------

-- TODO undo/redo
-- TODO multitouch
-- TODO fill out advanced params
-- TODO kbd shortcuts
-- TODO kbd shortcuts help text/popup
-- TODO have radio button choice for which property is controlled by x/y
-- TODO change poly colour based on cur settings
-- TODO set poly colour
-- TODO save presets
-- TODO delete poly
-- TODO save image
-- TODO save replay
-- TODO setting for resolution
-- TODO mode to make split with pointer start/end pos
-- TODO hide control panel
-- TODO add splash screen for first load
-- TODO fullscreen
-- TODO base colours on an image (specify how many colours to take from the image)
-- TODO hover over poly and get a color picker
-- TODO? maybe probability histogram for hue and lightness?

-------------------------------------------------------------------------------
-- Main -----------------------------------------------------------------------
-------------------------------------------------------------------------------

main :: Effect Unit
main = HA.runHalogenAff do
    HA.awaitLoad
    HA.selectElement (QuerySelector "#control_panel") >>= traverse_ (runUI component unit)

component :: forall m. MonadAff m => H.Component HH.HTML (Const Void) Unit Void m
component = H.mkComponent
    { initialState: const initial_state
    , render
    , eval: H.mkEval $ H.defaultEval
        { initialize = Just Initialize
        , handleAction = handle_action
        }
    }

type M = H.HalogenM State Action () Void

type Html m = H.ComponentHTML Action () m

{-- ------------------------------------------------------------------------------- --}
-- State ----------------------------------------------------------------------
-------------------------------------------------------------------------------

type State =
    { simple_params :: Simple_params
    , advanced_params :: Advanced_params
    , param_mode :: Param_mode
    , interaction_mode :: Interaction_mode
    , px_pct :: Number
    , py_pct :: Number
    , pointer_is_down :: Boolean
    , draw_debug_lines :: Boolean
    , draw_pointer_crosshair :: Boolean
    }

data Param_mode = Simple | Advanced

data Interaction_mode
    = Cut_poly_at_pointer
    | Cut_largest_poly
    | Cut_random_poly

initial_state :: State
initial_state =
    { simple_params: initial_simple_params
    , advanced_params: initial_advanced_params
    , param_mode: Simple
    , interaction_mode: Cut_poly_at_pointer
    , px_pct: 0.0
    , py_pct: 0.0
    , pointer_is_down: false
    , draw_debug_lines: false
    , draw_pointer_crosshair: false
    }

newtype Simple_params = Simple_params
    { cut_speed :: SimpleCutSpeedParam
    , cut_angle :: SimpleCutAngleParam
    , colour_change :: SimpleColourChangeParam
    , lightness_change :: SimpleLightnessChangeParam
    }

initial_simple_params :: Simple_params
initial_simple_params = Simple_params
    { cut_speed: Med
    , cut_angle: A_bit_uneven
    , colour_change: Moderate
    , lightness_change: Get_lighter
    }

newtype Advanced_params = Advanced_params (Record Params_R)

type Raw_params =
    { draw_debug_lines :: Boolean
    , draw_pointer_crosshair :: Boolean
    | Params_R
    }

type Params_R =
    ( n_cuts_per_tick :: Int
    , cut_ratio :: Number -- [0,1]
    , hue_delta :: Number -- [0,1]
    , hue_delta_variance :: Number
    , lightness_delta :: Number
    , lightness_delta_variance :: Number
    )

data SimpleCutSpeedParam = Slow | Med | Fast
data SimpleCutAngleParam = Very_uneven | A_bit_uneven | Even
data SimpleColourChangeParam = Tiny | Moderate | Pretty_much_random
data SimpleLightnessChangeParam = Get_darker | Stay_same | Get_lighter

initial_advanced_params :: Advanced_params
initial_advanced_params = Advanced_params
    { n_cuts_per_tick: 3
    , cut_ratio: 0.3
    , hue_delta: 0.0
    , hue_delta_variance: 0.02
    , lightness_delta: 0.01
    , lightness_delta_variance: 0.0
    }

-------------------------------------------------------------------------------
-- Render ---------------------------------------------------------------------
-------------------------------------------------------------------------------

render :: forall m. State -> Html m
render st = HH.div_
    [ render_control_panel_section "Interaction mode"
        [ button_bar $ [Cut_poly_at_pointer, Cut_largest_poly, Cut_random_poly] # map \x -> HH.button
            [ HE.onClick \_ -> Just $ Set_interaction_mode x
            , HP.classes $ [ ClassName "btn_bar_btn" ] <> if x == st.interaction_mode then [ ClassName "focused_button" ] else []
            ]
            [ HH.text case x of
                Cut_poly_at_pointer -> "Cut poly at pointer"
                Cut_largest_poly -> "Cut largest poly"
                Cut_random_poly -> "Cut random poly"
            ]
        ]
    , render_control_panel_section "Misc"
        [ HH.button
            [ HE.onClick \_ -> Just $ Set_param_mode case st.param_mode of
                Simple -> Advanced
                Advanced -> Simple
            ]
            [ HH.text case st.param_mode of
                Simple -> "Switch to advanced control panel"
                Advanced -> "Switch to simple control panel"
            ]
        , HH.br_
        , HH.label_
            [ HH.input
                [ HP.type_ InputCheckbox
                , HP.checked st.draw_debug_lines
                , HE.onClick \_ -> Just $ Set_param Toggle_draw_debug_lines
                ]
            , HH.text "Draw debug lines"
            ]
        , HH.br_
        , HH.label_
            [ HH.input
                [ HP.type_ InputCheckbox
                , HP.checked st.draw_pointer_crosshair
                , HE.onClick \_ -> Just $ Set_param Toggle_draw_pointer_crosshair
                ]
            , HH.text "Draw pointer crosshair"
            ]
        , HH.br_
        , HH.button
            [ HE.onClick \_ -> Just Reset_canvas ]
            [ HH.text "Reset canvas" ]
        ]
    , case st.param_mode of
        Simple -> render_simple_params st.simple_params
        Advanced -> render_advanced_params st.advanced_params
    ]

render_simple_params :: forall m. Simple_params -> Html m
render_simple_params (Simple_params params) = HH.div []
    [ render_control_panel_section "Cut speed"
        [ button_bar $ [Slow, Med, Fast] # map \x -> HH.button
            [ HE.onClick \_ -> Just $ Set_param $ Set_simple_cut_speed x
            , HP.classes $ [ ClassName "btn_bar_btn" ] <> if x == params.cut_speed then [ ClassName "focused_button" ] else []
            ]
            [ HH.text case x of
                Slow -> "Slow"
                Med -> "Med"
                Fast -> "Fast"
            ]
        ]
    , render_control_panel_section "Cut angle"
        [ button_bar $ [Very_uneven, A_bit_uneven, Even] # map \x -> HH.button
            [ HE.onClick \_ -> Just $ Set_param $ Set_simple_cut_angle x
            , HP.classes $ [ ClassName "btn_bar_btn" ] <> if x == params.cut_angle then [ ClassName "focused_button" ] else []
            ]
            [ HH.text case x of
                Very_uneven -> "Very uneven"
                A_bit_uneven -> "A bit uneven"
                Even -> "Even"
            ]
        ]
    , render_control_panel_section "Colour change"
        [ button_bar $ [Tiny, Moderate, Pretty_much_random] # map \x -> HH.button
            [ HE.onClick \_ -> Just $ Set_param $ Set_simple_colour_change x
            , HP.classes $ [ ClassName "btn_bar_btn" ] <> if x == params.colour_change then [ ClassName "focused_button" ] else []
            ]
            [ HH.text case x of
                Tiny -> "Tiny"
                Moderate -> "Moderate"
                Pretty_much_random -> "Pretty much random"
            ]
        ]
    , render_control_panel_section "Lightness change"
        [ button_bar $ [Get_darker, Stay_same, Get_lighter] # map \x -> HH.button
            [ HE.onClick \_ -> Just $ Set_param $ Set_simple_lightness_change x
            , HP.classes $ [ ClassName "btn_bar_btn" ] <> if x == params.lightness_change then [ ClassName "focused_button" ] else []
            ]
            [ HH.text case x of
                Get_darker -> "Get darker"
                Stay_same -> "Stay the same"
                Get_lighter -> "Get lighter"
            ]
        ]
    ]

render_advanced_params :: forall m. Advanced_params -> Html m
render_advanced_params (Advanced_params params) = HH.div_
    [ render_control_panel_section "# cuts per tick"
        [ HH.input
            [ HP.type_ InputRange
            , HP.min 1.0
            , HP.max 30.0
            , HP.step $ Step 1.0
            , HP.value $ show params.n_cuts_per_tick
            , HE.onValueInput \str -> do
                n <- Int.fromString str
                pure $ Set_param $ Set_advanced_n_cuts_per_tick n
            ]
        , HH.text $ show params.n_cuts_per_tick
        ]
    , render_control_panel_section "Cut ratio"
        [ HH.input
            [ HP.type_ InputRange
            , HP.min 0.0
            , HP.max 1.0
            , HP.step $ Step 0.05
            , HP.value $ show params.cut_ratio
            , HE.onValueInput \str -> do
                n <- Number.fromString str
                pure $ Set_param $ Set_advanced_cut_ratio n
            ]
        , HH.text $ show params.cut_ratio
        ]
    , render_control_panel_section "Hue delta"
        let hue_delta_display_scale = 1000.0
        in
        [ HH.input
            [ HP.type_ InputRange
            , HP.min (-1.0)
            , HP.max 1.0
            , HP.step $ Step 0.002
            , HP.value $ show $ params.hue_delta * hue_delta_display_scale
            , HE.onValueInput \str -> do
                n <- Number.fromString str
                pure $ Set_param $ Set_advanced_hue_delta $ n / hue_delta_display_scale
            ]
        , HH.text $ show $ params.hue_delta
        ]
    , render_control_panel_section "Hue delta variance"
        let hue_delta_variance_display_scale = 10.0
        in
        [ HH.input
            [ HP.type_ InputRange
            , HP.min 0.0
            , HP.max 1.0
            , HP.step $ Step 0.01
            , HP.value $ show $ params.hue_delta_variance * hue_delta_variance_display_scale
            , HE.onValueInput \str -> do
                n <- Number.fromString str
                pure $ Set_param $ Set_advanced_hue_delta_variance $ n / hue_delta_variance_display_scale
            ]
        , HH.text $ show $ params.hue_delta_variance
        ]
    , render_control_panel_section "Lightness delta"
        let lightness_delta_display_scale = 100.0
        in
        [ HH.input
            [ HP.type_ InputRange
            , HP.min (-1.0)
            , HP.max 1.0
            , HP.step $ Step 0.002
            , HP.value $ show $ params.lightness_delta * lightness_delta_display_scale
            , HE.onValueInput \str -> do
                n <- Number.fromString str
                pure $ Set_param $ Set_advanced_lightness_delta $ n / lightness_delta_display_scale
            ]
        , HH.text $ show params.lightness_delta
        ]
    , render_control_panel_section "Lightness delta variance"
        let lightness_delta_variance_display_scale = 10.0
        in
        [ HH.input
            [ HP.type_ InputRange
            , HP.min 0.0
            , HP.max 1.0
            , HP.step $ Step 0.01
            , HP.value $ show $ params.lightness_delta_variance * lightness_delta_variance_display_scale
            , HE.onValueInput \str -> do
                n <- Number.fromString str
                pure $ Set_param $ Set_advanced_lightness_delta_variance $ n / lightness_delta_variance_display_scale
            ]
        , HH.text $ show params.lightness_delta_variance
        ]
    ]

render_control_panel_section :: forall m. String -> Array (Html m) -> Html m
render_control_panel_section title children = HH.div
    [ HP.classes [ ClassName "control_panel_section" ] ]
    [ HH.div [ HP.classes [ ClassName "width100" ] ]
        [ HH.h4 [] [ HH.text title ]
        , HH.div [ HP.classes [ ClassName "width100" ] ] children
        ]
    ]

button_bar :: forall m. Array (Html m) -> Html m
button_bar btns = HH.span [ HP.classes [ ClassName "btn_bar" ] ] btns

-------------------------------------------------------------------------------
-- Action ---------------------------------------------------------------------
-------------------------------------------------------------------------------

data Action
    = Initialize
    | Handle_pointer_move Int Int
    | Handle_pointer_down
    | Handle_pointer_up
    | Set_interaction_mode Interaction_mode
    | Set_param_mode Param_mode
    | Set_param Set_param_action
    | Reset_canvas

data Set_param_action
    = Set_simple_cut_speed SimpleCutSpeedParam
    | Set_simple_cut_angle SimpleCutAngleParam
    | Set_simple_colour_change SimpleColourChangeParam
    | Set_simple_lightness_change SimpleLightnessChangeParam
    | Set_advanced_n_cuts_per_tick Int
    | Set_advanced_cut_ratio Number
    | Set_advanced_hue_delta Number
    | Set_advanced_hue_delta_variance Number
    | Set_advanced_lightness_delta Number
    | Set_advanced_lightness_delta_variance Number
    | Toggle_draw_debug_lines
    | Toggle_draw_pointer_crosshair

-------------------------------------------------------------------------------
-- Update ---------------------------------------------------------------------
-------------------------------------------------------------------------------

handle_action :: forall m.
    MonadAff m =>
    Action -> M m Unit
handle_action = case _ of
    Initialize -> do
        st <- H.get
        liftEffect set_action_no_op
        liftEffect $ set_params st
        liftEffect init

        canvas_container <- liftEffect get_canvas_container_event_target

        -- subscribe to mouse events
        void $ H.subscribe $ eventListenerEventSource mousemove canvas_container \evt -> do
            mevt <- ME.fromEvent evt
            pure $ Handle_pointer_move (ME.clientX mevt) (ME.clientY mevt)
        void $ H.subscribe $ eventListenerEventSource mousedown canvas_container \_ -> Just Handle_pointer_down
        void $ H.subscribe $ eventListenerEventSource mouseup canvas_container \_ -> Just Handle_pointer_up

        -- subscribe to touch events
        void $ H.subscribe $ eventListenerEventSource touchmove canvas_container \evt -> do
            tevt <- TE.fromEvent evt
            let changedTouches = TE.changedTouches tevt
            touch <- TL.item 0 changedTouches
            pure $ Handle_pointer_move (T.clientX touch) (T.clientY touch)
        void $ H.subscribe $ eventListenerEventSource touchstart canvas_container \_ -> Just Handle_pointer_down
        void $ H.subscribe $ eventListenerEventSource touchend canvas_container \_ -> Just Handle_pointer_up
        void $ H.subscribe $ eventListenerEventSource touchcancel canvas_container \_ -> Just Handle_pointer_up

    Handle_pointer_move x y -> do
        { px_pct, py_pct } <- liftEffect $ runFn2 update_px_py x y
        H.modify_ _ { px_pct = Int.toNumber x, py_pct = Int.toNumber y }

    Handle_pointer_down -> do
        st <- H.get
        case st.interaction_mode of
            Cut_poly_at_pointer -> liftEffect set_action_cut_poly_at_pointer
            Cut_largest_poly -> liftEffect set_action_cut_largest_poly
            Cut_random_poly -> liftEffect set_action_cut_random_poly
        H.put $ st { pointer_is_down = true }

    Handle_pointer_up -> do
        liftEffect set_action_no_op
        H.modify_ _ { pointer_is_down = false }

    Set_interaction_mode m -> H.modify_ $ Lens.set _interaction_mode m

    Set_param_mode m -> do
        H.modify_ $ Lens.set _param_mode m
        st <- H.get
        liftEffect $ set_params st

    Set_param a -> do
        handle_set_param_action a
        st <- H.get
        liftEffect $ set_params st

    Reset_canvas -> liftEffect reset_canvas

handle_set_param_action :: forall m. MonadEffect m => Set_param_action -> M m Unit
handle_set_param_action = case _ of
    Set_simple_cut_speed x -> H.modify_ $ Lens.set _simple_cut_speed x
    Set_simple_cut_angle x -> H.modify_ $ Lens.set _simple_cut_angle x
    Set_simple_colour_change x -> H.modify_ $ Lens.set _simple_colour_change x
    Set_simple_lightness_change x -> H.modify_ $ Lens.set _simple_lightness_change x
    Set_advanced_n_cuts_per_tick n -> H.modify_ $ Lens.set _advanced_n_cuts_per_tick n
    Set_advanced_cut_ratio n -> H.modify_ $ Lens.set _advanced_cut_ratio n
    Set_advanced_hue_delta n -> H.modify_ $ Lens.set _advanced_hue_delta n
    Set_advanced_hue_delta_variance n -> H.modify_ $ Lens.set _advanced_hue_delta_variance n
    Set_advanced_lightness_delta n -> H.modify_ $ Lens.set _advanced_lightness_delta n
    Set_advanced_lightness_delta_variance n -> H.modify_ $ Lens.set _advanced_lightness_delta_variance n
    Toggle_draw_debug_lines -> H.modify_ $ Lens.over _draw_debug_lines not
    Toggle_draw_pointer_crosshair -> H.modify_ $ Lens.over _draw_pointer_crosshair not

set_params :: State -> Effect Unit
set_params st = set_params_impl case st.param_mode of
    Simple ->
        let Simple_params ps = st.simple_params
        in
        { n_cuts_per_tick: case ps.cut_speed of
            Slow -> 1
            Med -> 5
            Fast -> 20
        , cut_ratio: case ps.cut_angle of
            Very_uneven -> 0.25
            A_bit_uneven -> 0.4
            Even -> 1.0
        , hue_delta: case ps.colour_change of
            Tiny -> 0.00001
            Moderate -> 0.0003
            Pretty_much_random -> 0.001
        , hue_delta_variance: case ps.colour_change of
            Tiny -> 0.002
            Moderate -> 0.03
            Pretty_much_random -> 0.1
        , lightness_delta: case ps.cut_angle, ps.lightness_change of
            Very_uneven, Get_darker -> -0.001
            Very_uneven, Get_lighter -> 0.001
            A_bit_uneven, Get_darker -> -0.004
            A_bit_uneven, Get_lighter -> 0.004
            Even, Get_darker -> -0.03
            Even, Get_lighter -> 0.03
            _, Stay_same -> 0.0
        , lightness_delta_variance: 0.005
        , draw_debug_lines: st.draw_debug_lines
        , draw_pointer_crosshair: st.draw_pointer_crosshair
        }
    Advanced -> st.advanced_params
        # unwrap
        # Record.insert (SProxy :: SProxy "draw_debug_lines") st.draw_debug_lines
        # Record.insert (SProxy :: SProxy "draw_pointer_crosshair") st.draw_pointer_crosshair

foreign import get_canvas_container_event_target :: Effect EventTarget
foreign import update_px_py :: Fn2 Int Int (Effect { px_pct :: Number, py_pct :: Number })
foreign import set_action_no_op :: Effect Unit
foreign import set_action_cut_poly_at_pointer :: Effect Unit
foreign import set_action_cut_largest_poly :: Effect Unit
foreign import set_action_cut_random_poly :: Effect Unit
foreign import set_params_impl :: Raw_params -> Effect Unit
foreign import init :: Effect Unit
foreign import reset_canvas :: Effect Unit

-------------------------------------------------------------------------------
-- Lenses ---------------------------------------------------------------------
-------------------------------------------------------------------------------

_interaction_mode :: Lens' State Interaction_mode
_interaction_mode = prop (SProxy :: SProxy "interaction_mode")

_param_mode :: Lens' State Param_mode
_param_mode = prop (SProxy :: SProxy "param_mode")

_simple_params :: Lens' State Simple_params
_simple_params = prop (SProxy :: SProxy "simple_params")

_simple_cut_speed :: Lens' State SimpleCutSpeedParam
_simple_cut_speed = _simple_params <<< _Newtype <<< prop (SProxy :: SProxy "cut_speed")

_simple_cut_angle :: Lens' State SimpleCutAngleParam
_simple_cut_angle = _simple_params <<< _Newtype <<< prop (SProxy :: SProxy "cut_angle")

_simple_colour_change :: Lens' State SimpleColourChangeParam
_simple_colour_change = _simple_params <<< _Newtype <<< prop (SProxy :: SProxy "colour_change")

_simple_lightness_change :: Lens' State SimpleLightnessChangeParam
_simple_lightness_change = _simple_params <<< _Newtype <<< prop (SProxy :: SProxy "lightness_change")

_advanced_params :: Lens' State Advanced_params
_advanced_params = prop (SProxy :: SProxy "advanced_params")

_advanced_n_cuts_per_tick :: Lens' State Int
_advanced_n_cuts_per_tick = _advanced_params <<< _Newtype <<< prop (SProxy :: SProxy "n_cuts_per_tick")

_advanced_cut_ratio :: Lens' State Number
_advanced_cut_ratio = _advanced_params <<< _Newtype <<< prop (SProxy :: SProxy "cut_ratio")

_advanced_hue_delta :: Lens' State Number
_advanced_hue_delta = _advanced_params <<< _Newtype <<< prop (SProxy :: SProxy "hue_delta")

_advanced_hue_delta_variance :: Lens' State Number
_advanced_hue_delta_variance = _advanced_params <<< _Newtype <<< prop (SProxy :: SProxy "hue_delta_variance")

_advanced_lightness_delta :: Lens' State Number
_advanced_lightness_delta = _advanced_params <<< _Newtype <<< prop (SProxy :: SProxy "lightness_delta")

_advanced_lightness_delta_variance :: Lens' State Number
_advanced_lightness_delta_variance = _advanced_params <<< _Newtype <<< prop (SProxy :: SProxy "lightness_delta_variance")

_draw_debug_lines :: Lens' State Boolean
_draw_debug_lines = prop (SProxy :: SProxy "draw_debug_lines")

_draw_pointer_crosshair :: Lens' State Boolean
_draw_pointer_crosshair = prop (SProxy :: SProxy "draw_pointer_crosshair")

-------------------------------------------------------------------------------
-- Instances ------------------------------------------------------------------
-------------------------------------------------------------------------------

derive instance eqInteraction_mode :: Eq Interaction_mode

derive instance newtypeSimple_params :: Newtype Simple_params _
derive instance newtypeAdvanced_params :: Newtype Advanced_params _

derive instance eqSimpleCutSpeedParam :: Eq SimpleCutSpeedParam
derive instance eqSimpleCutAngleParam :: Eq SimpleCutAngleParam
derive instance eqSimpleLightnessChangeParam :: Eq SimpleLightnessChangeParam
derive instance eqSimpleColourChangeParam :: Eq SimpleColourChangeParam
