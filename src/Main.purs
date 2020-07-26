module Main where

import Prelude

import DOM.HTML.Indexed.InputType (InputType(..))
import Data.Array as A
import Data.Const (Const)
import Data.Foldable (traverse_)
import Data.Function.Uncurried (Fn2, runFn2)
import Data.Int as Int
import Data.Lens (Lens')
import Data.Lens as Lens
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype)
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
import Math (abs)
import Slider (format_as_percentage)
import Slider as Slider
import Web.DOM.ParentNode (QuerySelector(..))
import Web.Event.Event (stopPropagation)
import Web.Event.EventTarget (EventTarget)
import Web.TouchEvent.EventTypes (touchcancel, touchend, touchmove, touchstart)
import Web.TouchEvent.Touch as T
import Web.TouchEvent.TouchEvent as TE
import Web.TouchEvent.TouchList as TL
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent as ME
import Web.UIEvent.MouseEvent.EventTypes (mousedown, mousemove, mouseup)

-------------------------------------------------------------------------------
-- TODOS ----------------------------------------------------------------------
-------------------------------------------------------------------------------

-- DONE save image

-- TODO add license
-- TODO undo/redo
-- TODO multitouch
-- TODO kbd shortcuts
-- TODO kbd shortcuts help text/popup
-- TODO have radio button choice for which property is controlled by x/y
-- TODO change poly colour based on cur settings
-- TODO set poly colour
-- TODO save presets
-- TODO delete poly
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
    HA.selectElement (QuerySelector "body") >>= traverse_ (runUI component unit)

component :: forall m. MonadAff m => H.Component HH.HTML (Const Void) Unit Void m
component = H.mkComponent
    { initialState: \_ ->
        { params: Params
            { n_cuts_per_tick: 3
            , cut_ratio: 0.12
            , hue_delta: { min: 0.0, max: 0.01 }
            , lightness_delta: { min: 0.0, max: 0.002 }
            }
        , interaction_mode: Cut_poly_at_pointer
        , px_pct: 0.0
        , py_pct: 0.0
        , pointer_is_down: false
        , draw_debug_lines: false
        , draw_pointer_crosshair: false
        , m_dialog_state: Nothing
        }
    , render
    , eval: H.mkEval $ H.defaultEval
        { initialize = Just Initialize
        , handleAction = handle_action
        }
    }

type M = H.HalogenM State Action Child_slots Void

type Html m = H.ComponentHTML Action Child_slots m

-------------------------------------------------------------------------------
-- State ----------------------------------------------------------------------
-------------------------------------------------------------------------------

type State =
    { params :: Params
    , interaction_mode :: Interaction_mode
    , px_pct :: Number
    , py_pct :: Number
    , pointer_is_down :: Boolean
    , draw_debug_lines :: Boolean
    , draw_pointer_crosshair :: Boolean
    , m_dialog_state :: Maybe Dialog_state
    }

data Interaction_mode
    = Cut_poly_at_pointer
    | Cut_largest_poly
    | Delete_poly_at_pointer
    | Change_poly_colour_at_pointer

newtype Params = Params
    { n_cuts_per_tick :: Int
    , cut_ratio :: Number -- [0,0.5]
    , hue_delta :: Number_range -- [-0.5,0.5]
    , lightness_delta :: Number_range -- [-1,1]
    }

type Number_range = { min :: Number, max :: Number }

data Dialog_state
    = DS_interaction_mode_explanation Interaction_mode
    | DS_n_cuts_per_tick_explanation
    | DS_cut_ratio_explanation Explanation_depth
    | DS_hue_delta_explanation
    | DS_lightness_delta_explanation

data Explanation_depth = Simple | Nerdy

-------------------------------------------------------------------------------
-- Slots ----------------------------------------------------------------------
-------------------------------------------------------------------------------

type Child_slots =
    ( slider :: Slider.Slot String -- id
    )

_slider = SProxy :: SProxy "slider"

-------------------------------------------------------------------------------
-- Render ---------------------------------------------------------------------
-------------------------------------------------------------------------------

render :: forall m. MonadAff m => State -> Html m
render st = HH.div
    [ HP.id_ "root" ]
    [ HH.div [ HP.id_ "canvas_container" ] []
    , render_control_panel st
    , render_dialog st
    ]

render_control_panel :: forall m. MonadAff m => State -> Html m
render_control_panel st = HH.div
    [ HP.id_ "control_panel" ]
    [ render_control_panel_section
        { m_on_help_clicked: Nothing
        , title: "Misc. options"
        , children:
            [ HH.label_
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
                [ HE.onClick \_ -> Just Save_as_png ]
                [ HH.text "Save as png" ]
            , HH.br_
            , HH.button
                [ HE.onClick \_ -> Just Reset_canvas ]
                [ HH.text "Reset canvas" ]
            ]
        }
    , render_control_panel_section
        { m_on_help_clicked: Nothing
        , title: "Interaction mode"
        , children:
            [ Cut_poly_at_pointer
            , Cut_largest_poly
            , Delete_poly_at_pointer
            , Change_poly_colour_at_pointer
            ] # map \im -> HH.div
                [ HP.class_ $ ClassName "interaction_mode_option" ]
                [ HH.label_
                    [ HH.input
                        [ HE.onClick \_ -> Just $ Set_interaction_mode im
                        , HP.checked $ st.interaction_mode == im
                        , HP.type_ InputRadio
                        ]
                    , HH.text $ print_interaction_mode im
                    ]
                , help_btn \e -> Set_dialog_state e $ Just $ DS_interaction_mode_explanation im
                ]
        }
    , render_params st.params
    ]

render_params :: forall m. MonadAff m => Params -> Html m
render_params (Params params) = HH.div_
    [ render_control_panel_section
        { m_on_help_clicked: Just \e -> Set_dialog_state e $ Just DS_n_cuts_per_tick_explanation
        , title: "# cuts per tick (" <> show params.n_cuts_per_tick <> ")"
        , children:
            [ HH.slot _slider "n_cuts_per_tick_slider" Slider.component
                { id: "n_cuts_per_tick_slider"
                , start: [ Int.toNumber params.n_cuts_per_tick ]
                , range:
                    [ { k: "min", v: 1.0, step: 1.0 }
                    , { k: "max", v: 30.0, step: 1.0 }
                    ]
                , format:
                    { to: Int.floor >>> show
                    , from: Number.fromString >>> fromMaybe 1.0
                    }
                } case _ of
                    Slider.Slider_updated arr -> do
                        v <- A.index arr 0
                        pure $ Set_param $ Set_n_cuts_per_tick $ Int.floor v
            ]
        }
    , render_control_panel_section
        { m_on_help_clicked: Just \e -> Set_dialog_state e $ Just $ DS_cut_ratio_explanation Simple
        , title: "Cut ratio (" <> (format_as_percentage $ const 0).to params.cut_ratio <> ")"
        , children:
            [ HH.slot _slider "cut_ratio_slider" Slider.component
                { id: "cut_ratio_slider"
                , start: [ params.cut_ratio ]
                , range:
                    [ { k: "min", v: 0.0, step: 0.01 }
                    , { k: "max", v: 0.5, step: 0.01 }
                    ]
                , format: format_as_percentage $ const 0
                } case _ of
                    Slider.Slider_updated arr -> do
                        n <- A.index arr 0
                        pure $ Set_param $ Set_cut_ratio n
            ]
        }
    , render_control_panel_section
        { m_on_help_clicked: Just \e -> Set_dialog_state e $ Just DS_hue_delta_explanation
        , title: "Colour change " <> render_range n_decimal_places_for_hue_and_lightness params.hue_delta
        , children:
            [ HH.slot _slider "hue_delta_slider" Slider.component
                { id: "hue_delta_slider"
                , start: [ params.hue_delta.min, params.hue_delta.max ]
                , range:
                    [ { k: "min", v: -0.5, step: 0.001 }
                    , { k: "25%", v: -0.001, step: 0.00001 }
                    , { k: "50%", v: 0.0, step: 0.00001 }
                    , { k: "75%", v: 0.001, step: 0.001 }
                    , { k: "max", v: 0.5, step: 0.001 }
                    ]
                , format: format_as_percentage n_decimal_places_for_hue_and_lightness
                } case _ of
                    Slider.Slider_updated arr -> do
                        u <- A.index arr 0
                        v <- A.index arr 1
                        pure $ Set_param $ Set_hue_delta { min: u, max: v }
            ]
        }
    , render_control_panel_section
        { m_on_help_clicked: Just \e -> Set_dialog_state e $ Just DS_lightness_delta_explanation
        , title: "Lightness change " <> render_range n_decimal_places_for_hue_and_lightness params.lightness_delta
        , children:
            [ HH.slot _slider "lightness_delta_slider" Slider.component
                { id: "lightness_delta_slider"
                , start: [ params.lightness_delta.min, params.lightness_delta.max ]
                , range:
                    [ { k: "min", v: -0.5, step: 0.001 }
                    , { k: "25%", v: -0.001, step: 0.00001 }
                    , { k: "50%", v: 0.0, step: 0.00001 }
                    , { k: "75%", v: 0.001, step: 0.001 }
                    , { k: "max", v: 0.5, step: 0.001 }
                    ]
                , format: format_as_percentage n_decimal_places_for_hue_and_lightness
                } case _ of
                    Slider.Slider_updated arr -> do
                        u <- A.index arr 0
                        v <- A.index arr 1
                        pure $ Set_param $ Set_lightness_delta { min: u, max: v }
            ]
        }
    ]

n_decimal_places_for_hue_and_lightness :: Number -> Int
n_decimal_places_for_hue_and_lightness n =
    let absn = abs n
    in if absn == 0.0 then 0
    else if absn < 0.1 then 3
    else if absn < 50.0 then 1
    else 0

render_range :: (Number -> Int) -> Number_range -> String
render_range n_decimal_places r =
    let format = format_as_percentage n_decimal_places
        u = format.to r.min
        v = format.to r.max
    in "(" <> u <> ", " <> v <> ")"

render_control_panel_section :: forall m.
    { m_on_help_clicked :: Maybe (MouseEvent -> Action)
    , title :: String
    , children :: Array (Html m)
    } -> Html m
render_control_panel_section { m_on_help_clicked, title, children } = HH.div
    [ HP.classes [ ClassName "control_panel_section" ] ]
    [ HH.div [ HP.classes [ ClassName "width100" ] ]
        [ HH.h4
            [ HP.class_ $ ClassName "control_panel_section_header" ] $
            [ HH.text title ]
            <> case m_on_help_clicked of
                Nothing -> []
                Just on_help_clicked -> [ help_btn on_help_clicked ]
        , HH.div [ HP.classes [ ClassName "width100" ] ] children
        ]
    ]

help_btn :: forall m. (MouseEvent -> Action) -> Html m
help_btn on_click = HH.i
    [ HP.classes $ map ClassName [ "fas", "fa-question-circle", "control_panel_help_btn" ]
    , HP.tabIndex 0
    , HE.onClick \e -> Just $ on_click e
    ]
    []

button_bar :: forall m. Array (Html m) -> Html m
button_bar btns = HH.span [ HP.classes [ ClassName "btn_bar" ] ] btns

render_dialog :: forall m. State -> Html m
render_dialog st = case st.m_dialog_state of
    Nothing -> HH.div_ []
    Just ds -> render_dialog_ st ds

render_dialog_ :: forall m. State -> Dialog_state -> Html m
render_dialog_ st ds =
    let cut_param_paragraphs =
            [ HH.p_
                [ HH.text "The "
                , HH.b_ [ HH.text "Cut ratio" ]
                , HH.text " parameter controls how even the pieces are in terms of area."
                ]
            , HH.p_
                [ HH.text "The "
                , HH.b_ [ HH.text "Colour change" ]
                , HH.text " parameter controls how the colour of the smaller polygons change relative to the colour of the original polygon."
                ]
            , HH.p_
                [ HH.text "The "
                , HH.b_ [ HH.text "Lightness change" ]
                , HH.text " parameter controls how the lightness of the smaller polygons change relative to the lightness of the original polygon."
                ]
            ]
    in HH.div
    [ HP.id_ "dialog_wrapper"
    , HE.onClick \e -> Just $ Set_dialog_state e Nothing
    ]
    [ HH.div
        [ HP.id_ "dialog" ]
        [ HH.h3
            [ HP.id_ "dialog_header" ]
            [ HH.text case ds of
                DS_interaction_mode_explanation im -> print_interaction_mode im
                DS_n_cuts_per_tick_explanation -> "# cuts per tick"
                DS_cut_ratio_explanation _ -> "Cut ratio"
                DS_hue_delta_explanation -> "Colour change"
                DS_lightness_delta_explanation -> "Lightness delta"
            , HH.i
                [ HP.classes $ map ClassName [ "fas", "fa-times", "close-btn" ]
                , HP.tabIndex 0
                , HE.onClick \e -> Just $ Set_dialog_state e Nothing
                ]
                []
            ]
        , HH.div [ HP.id_ "dialog_contents" ] case ds of
            DS_interaction_mode_explanation Cut_poly_at_pointer ->
                [ HH.p_ [ HH.text "Clicking/touching a polygon will cut it into two smaller polygons." ] ] <> cut_param_paragraphs

            DS_interaction_mode_explanation Cut_largest_poly ->
                [ HH.p_ [ HH.text "Clicking/touching anywhere on the canvas will cut the largest polygon into two smaller polygons." ]
                ] <> cut_param_paragraphs

            DS_interaction_mode_explanation Delete_poly_at_pointer ->
                [ HH.text "Clicking/touching a polygon will delete it." ]

            DS_interaction_mode_explanation Change_poly_colour_at_pointer ->
                [ HH.text "The polygon being clicked/touched will change colour and lightness based on the "
                , HH.b_ [ HH.text "Colour change" ]
                , HH.text " parameter and the "
                , HH.b_ [ HH.text "Lightness change" ]
                , HH.text " parameter."
                ]

            DS_n_cuts_per_tick_explanation ->
                [ HH.p_ [ HH.text "The number of times a new polygon is selected and cut each tick (there are roughly 60 ticks every second)." ]
                , HH.p_ [ HH.b_ [ HH.text "TLDR:" ], HH.text " speed (the higher the number, the faster the cuts get made)." ]
                ]

            DS_cut_ratio_explanation Simple ->
                [ HH.p_ [ HH.text "How even the cut is." ]
                , HH.p_ [ HH.text "Closer to 0% will make the cut less even, and closer to 50% will make the cut more even." ]
                , HH.p_ [ HH.button [ HE.onClick \e -> Just $ Set_dialog_state e $ Just $ DS_cut_ratio_explanation Nerdy ] [ HH.text "Click for nerdy explanation" ] ]
                ]

            DS_cut_ratio_explanation Nerdy ->
                [ HH.p_
                    [ HH.text "1. A point "
                    , HH.b_ [ HH.text "P" ]
                    , HH.text " is chosen at random along the circumference of the polygon."
                    ]
                , HH.p_
                    [ HH.text "2. A second point "
                    , HH.b_ [ HH.text "Q" ]
                    , HH.text " is calculated by moving "
                    , HH.b_ [ HH.text "Cut ratio %" ]
                    , HH.text " along the circumference of the polygon from "
                    , HH.b_ [ HH.text "P" ]
                    , HH.text "."
                    ]
                , HH.p_
                    [ HH.text "3. The polygon is cut along the line "
                    , HH.b_ [ HH.text "PQ" ]
                    , HH.text "."
                    ]
                ]

            DS_hue_delta_explanation ->
                [
                ]

            DS_lightness_delta_explanation -> []
        ]
    ]

print_interaction_mode :: Interaction_mode -> String
print_interaction_mode = case _ of
    Cut_poly_at_pointer -> "Cut poly at pointer"
    Cut_largest_poly -> "Cut largest poly"
    Delete_poly_at_pointer -> "Delete poly at pointer"
    Change_poly_colour_at_pointer -> "Change colour of poly at pointer"

-------------------------------------------------------------------------------
-- Action ---------------------------------------------------------------------
-------------------------------------------------------------------------------

data Action
    = Initialize
    | Handle_pointer_move Int Int
    | Handle_pointer_down Int Int
    | Handle_pointer_up
    | Set_interaction_mode Interaction_mode
    | Set_param Set_param_action
    | Save_as_png
    | Reset_canvas
    | Set_dialog_state MouseEvent (Maybe Dialog_state)

data Set_param_action
    = Set_n_cuts_per_tick Int
    | Set_cut_ratio Number
    | Set_hue_delta Number_range
    | Set_lightness_delta Number_range
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
        void $ H.subscribe $ eventListenerEventSource mousedown canvas_container \evt -> do
            mevt <- ME.fromEvent evt
            pure $ Handle_pointer_down (ME.clientX mevt) (ME.clientY mevt)
        void $ H.subscribe $ eventListenerEventSource mouseup canvas_container \_ -> Just Handle_pointer_up

        -- subscribe to touch events
        void $ H.subscribe $ eventListenerEventSource touchmove canvas_container \evt -> do
            tevt <- TE.fromEvent evt
            let changedTouches = TE.changedTouches tevt
            touch <- TL.item 0 changedTouches
            pure $ Handle_pointer_move (T.clientX touch) (T.clientY touch)
        void $ H.subscribe $ eventListenerEventSource touchstart canvas_container \evt -> do
            tevt <- TE.fromEvent evt
            let changedTouches = TE.changedTouches tevt
            touch <- TL.item 0 changedTouches
            pure $ Handle_pointer_down (T.clientX touch) (T.clientY touch)
        void $ H.subscribe $ eventListenerEventSource touchend canvas_container \_ -> Just Handle_pointer_up
        void $ H.subscribe $ eventListenerEventSource touchcancel canvas_container \_ -> Just Handle_pointer_up

    Handle_pointer_move x y -> do
        { px_pct, py_pct } <- liftEffect $ runFn2 update_px_py x y
        H.modify_ _ { px_pct = Int.toNumber x, py_pct = Int.toNumber y }

    Handle_pointer_down x y -> do
        { px_pct, py_pct } <- liftEffect $ runFn2 update_px_py x y
        st <- H.get
        case st.interaction_mode of
            Cut_poly_at_pointer -> liftEffect set_action_cut_poly_at_pointer
            Cut_largest_poly -> liftEffect set_action_cut_largest_poly
            Delete_poly_at_pointer -> liftEffect set_action_delete_poly_at_pointer
            Change_poly_colour_at_pointer -> liftEffect set_action_change_poly_colour_at_pointer
        H.put $ st { px_pct = Int.toNumber x, py_pct = Int.toNumber y, pointer_is_down = true }

    Handle_pointer_up -> do
        liftEffect set_action_no_op
        H.modify_ _ { pointer_is_down = false }

    Set_interaction_mode m -> H.modify_ $ Lens.set _interaction_mode m

    Set_param a -> do
        handle_set_param_action a
        st <- H.get
        liftEffect $ set_params st

    Save_as_png -> liftEffect save_as_png

    Reset_canvas -> liftEffect reset_canvas

    Set_dialog_state e m -> do
        liftEffect $ stopPropagation $ ME.toEvent e
        H.modify_ $ Lens.set (prop (SProxy :: SProxy "m_dialog_state")) m

handle_set_param_action :: forall m. MonadEffect m => Set_param_action -> M m Unit
handle_set_param_action = case _ of
    Set_n_cuts_per_tick n -> H.modify_ $ Lens.set _n_cuts_per_tick n
    Set_cut_ratio n -> H.modify_ $ Lens.set _cut_ratio n
    Set_hue_delta r -> H.modify_ $ Lens.set _hue_delta r
    Set_lightness_delta r -> H.modify_ $ Lens.set _lightness_delta r
    Toggle_draw_debug_lines -> H.modify_ $ Lens.over _draw_debug_lines not
    Toggle_draw_pointer_crosshair -> H.modify_ $ Lens.over _draw_pointer_crosshair not

foreign import get_canvas_container_event_target :: Effect EventTarget
foreign import update_px_py :: Fn2 Int Int (Effect { px_pct :: Number, py_pct :: Number })
foreign import set_action_no_op :: Effect Unit
foreign import set_action_cut_poly_at_pointer :: Effect Unit
foreign import set_action_cut_largest_poly :: Effect Unit
foreign import set_action_delete_poly_at_pointer :: Effect Unit
foreign import set_action_change_poly_colour_at_pointer :: Effect Unit
foreign import set_params :: State -> Effect Unit
foreign import init :: Effect Unit
foreign import reset_canvas :: Effect Unit
foreign import save_as_png :: Effect Unit

-------------------------------------------------------------------------------
-- Lenses ---------------------------------------------------------------------
-------------------------------------------------------------------------------

_interaction_mode :: Lens' State Interaction_mode
_interaction_mode = prop (SProxy :: SProxy "interaction_mode")

_params :: Lens' State Params
_params = prop (SProxy :: SProxy "params")

_n_cuts_per_tick :: Lens' State Int
_n_cuts_per_tick = _params <<< _Newtype <<< prop (SProxy :: SProxy "n_cuts_per_tick")

_cut_ratio :: Lens' State Number
_cut_ratio = _params <<< _Newtype <<< prop (SProxy :: SProxy "cut_ratio")

_hue_delta :: Lens' State Number_range
_hue_delta = _params <<< _Newtype <<< prop (SProxy :: SProxy "hue_delta")

_lightness_delta :: Lens' State Number_range
_lightness_delta = _params <<< _Newtype <<< prop (SProxy :: SProxy "lightness_delta")

_draw_debug_lines :: Lens' State Boolean
_draw_debug_lines = prop (SProxy :: SProxy "draw_debug_lines")

_draw_pointer_crosshair :: Lens' State Boolean
_draw_pointer_crosshair = prop (SProxy :: SProxy "draw_pointer_crosshair")

-------------------------------------------------------------------------------
-- Instances ------------------------------------------------------------------
-------------------------------------------------------------------------------

derive instance eqInteraction_mode :: Eq Interaction_mode
derive instance newtypeParams :: Newtype Params _
