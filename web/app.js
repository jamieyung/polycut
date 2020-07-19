// TODOS ======================================================================

// TODO kbd shortcuts help text/popup
// TODO have radio button choice for which property is controlled by x/y
// TODO change poly colour based on cur settings
// TODO set poly colour
// TODO save presets
// TODO delete poly
// TODO save image
// TODO save replay
// TODO setting for resolution
// TODO mode to make split with pointer start/end pos
// TODO hide control panel
// TODO add simple/advanced control panel modes
// TODO add splash screen for first load
// TODO fullscreen
// TODO base colours on an image (specify how many colours to take from the image)
// TODO hover over poly and get a color picker

// CONSTANTS ==================================================================

const trace = console.log

const DIM = 800
const DIM_2 = DIM/2
const TWO_PI = Math.PI*2

const MODE = {
  DEFAULT: 0,
  CONTROL_SETTINGS: 1,
  SPLIT_POLY_AT_POINTER: 2,
  CHANGE_POLY_COLOUR_AT_POINTER: 3,
  DELETE_POLY_AT_POINTER: 4,
}
const N_MODES = Object.keys(MODE).length
const MODE_MIN = Object.entries(MODE)[0][1]
const MODE_MAX = Object.entries(MODE)[N_MODES - 1][1]

const RATIO_MIN = 0.05
const RATIO_MAX = 1
const RATIO_INITIAL_VAL = 0.25
const RATIO_RANGE = {
  "min": [RATIO_MIN],
  "max": [RATIO_MAX]
}
const RATIO_DISPLAY_SCALE = 1

const HUE_DELTA_MIN = -0.001
const HUE_DELTA_MAX = 0.001
const HUE_DELTA_INITIAL_VAL = 0
const HUE_DELTA_RANGE = {
  "min": [HUE_DELTA_MIN],
  "25%": [lerp(HUE_DELTA_MIN, HUE_DELTA_MAX, 0.4)],
  "75%": [lerp(HUE_DELTA_MIN, HUE_DELTA_MAX, 0.6)],
  "max": [HUE_DELTA_MAX]
}
const HUE_DELTA_DISPLAY_SCALE = 1000

const HUE_DELTA_VARIANCE_MIN = 0
const HUE_DELTA_VARIANCE_MAX = 0.1
const HUE_DELTA_VARIANCE_INITIAL_VAL = 0.002
const HUE_DELTA_VARIANCE_RANGE = {
  "min": [HUE_DELTA_VARIANCE_MIN],
  "50%": [lerp(HUE_DELTA_VARIANCE_MIN, HUE_DELTA_VARIANCE_MAX, 0.3)],
  "max": [HUE_DELTA_VARIANCE_MAX]
}
const HUE_DELTA_VARIANCE_DISPLAY_SCALE = 10

const LIGHTNESS_DELTA_MIN = -0.01
const LIGHTNESS_DELTA_MAX = 0.01
const LIGHTNESS_DELTA_INITIAL_VAL = 0.0008
const LIGHTNESS_DELTA_RANGE = {
  "min": [LIGHTNESS_DELTA_MIN],
  "25%": [lerp(LIGHTNESS_DELTA_MIN, LIGHTNESS_DELTA_MAX, 0.4)],
  "75%": [lerp(LIGHTNESS_DELTA_MIN, LIGHTNESS_DELTA_MAX, 0.6)],
  "max": [LIGHTNESS_DELTA_MAX]
}
const LIGHTNESS_DELTA_DISPLAY_SCALE = 100

const LIGHTNESS_DELTA_VARIANCE_MIN = 0
const LIGHTNESS_DELTA_VARIANCE_MAX = 0.1
const LIGHTNESS_DELTA_VARIANCE_INITIAL_VAL = 0.008
const LIGHTNESS_DELTA_VARIANCE_RANGE = {
  "min": [LIGHTNESS_DELTA_VARIANCE_MIN],
  "50%": [lerp(LIGHTNESS_DELTA_VARIANCE_MIN, LIGHTNESS_DELTA_VARIANCE_MAX, 0.3)],
  "max": [LIGHTNESS_DELTA_VARIANCE_MAX]
}
const LIGHTNESS_DELTA_VARIANCE_DISPLAY_SCALE = 10

const ACTION_SPEED_MIN = 1
const ACTION_SPEED_MAX = 50
const ACTION_SPEED_INITIAL_VAL = 5
const ACTION_SPEED_RANGE = {
  "min": [ACTION_SPEED_MIN, 1],
  "max": [ACTION_SPEED_MAX, 1]
}
const ACTION_SPEED_DISPLAY_SCALE = 1

// VARIABLES ==================================================================

let resolution
let app
let canvas_container_el
let control_panel_el
let pointer_is_down // true if mouse/touch is down
let px_pct // pointer x as frac of DIM
let py_pct // pointer y as frac of DIM
let g // graphics
let history
let last_executed_command_idx // if > -1, means at state just after performing history[last_executed_command_idx]
let polygons
let lines
let settings

// SETUP ======================================================================

function main() {
  resolution = 2*(window.devicePixelRatio || 1)
  app = new PIXI.Application({
    width: DIM,
    height: DIM,
    backgroundColor: 0xffffff,
    resolution: resolution,
    antialias: true
  })
  canvas_container_el = document.getElementById("canvas_container")
  canvas_container_el.appendChild(app.view)
  control_panel_el = document.getElementById("control_panel")

  init_listeners()

  pointer_is_down = false

  px_pct = 0
  py_pct = 0

  g = new PIXI.Graphics()
  app.stage.addChild(g)

  polygons = []
  lines = []

  init_control_panel()

  resize()
  reset()

  app.ticker.add(tick)
}

function resize() {
  const w = window.innerWidth
  const h = window.innerHeight
  const window_ratio = w/h

  if (window_ratio < 0.8) { // very tall
    document.body.style["flex-direction"] = "column"
    canvas_container_el.style["flex-basis"] = w + "px"
  } else if (window_ratio > 1.5) { // very wide
    document.body.style["flex-direction"] = "row"
    canvas_container_el.style["flex-basis"] = h + "px"
  } else { // roughly square
    document.body.style["flex-direction"] = "row"
    canvas_container_el.style["flex-basis"] = "65%"
  }

  let rect = canvas_container_el.getBoundingClientRect()
  const canvas_len = 0.9*Math.min(rect.width, rect.height) + "px"
  app.view.style["max-width"] = canvas_len
  app.view.style["max-height"] = canvas_len
}

function reset() {
  settings.mode.set_value(MODE.SPLIT_POLY_AT_POINTER)
  settings.ratio.set_value(RATIO_INITIAL_VAL)
  settings.hue_delta.set_value(HUE_DELTA_INITIAL_VAL)
  settings.hue_delta_variance.set_value(HUE_DELTA_VARIANCE_INITIAL_VAL)
  settings.lightness_delta.set_value(LIGHTNESS_DELTA_INITIAL_VAL)
  settings.lightness_delta_variance.set_value(LIGHTNESS_DELTA_VARIANCE_INITIAL_VAL)
  settings.action_speed.set_value(ACTION_SPEED_INITIAL_VAL)
  settings.draw_debug_lines.set_value(false)

  reset_canvas()
}

function reset_canvas() {
  const p00 = pt(0, 0)
  const p10 = pt(DIM, 0)
  const p11 = pt(DIM, DIM)
  const p01 = pt(0, DIM)

  lines = [mk_line(p00, p10), mk_line(p10, p11), mk_line(p11, p01), mk_line(p01, p00)]

  const h = Math.random()
  const s = Math.random()
  const l = Math.random()
  const first_poly = mk_poly([p00, p10, p11, p01], h, s, l)
  polygons = [first_poly]

  history = []
  last_executed_command_idx = -1
}

// TICK =======================================================================

function tick() {
  g.clear()

  for (let i = 0; i < polygons.length; i++) {
    const poly = polygons[i]
    g.beginFill(poly.hex)
    g.drawPolygon(poly.verts)
    g.endFill()
  }

  if (settings.draw_debug_lines.get_value()) {
    g.lineStyle(1, 0xff00ff)
    for (let i = 0; i < lines.length; i++) {
      const line = lines[i]
      g.moveTo(line.u.x, line.u.y)
      g.lineTo(line.v.x, line.v.y)
    }
  }

  if (pointer_is_down) {
    do_action()
  }
}

function do_action() {
  const mode = settings.mode.get_value()
  if (mode === MODE.DEFAULT) {
    split_n_polys()
  } else if (mode === MODE.CONTROL_SETTINGS) {
    split_n_polys()
  } else if (mode === MODE.SPLIT_POLY_AT_POINTER) {
    const n = settings.action_speed.get_value()
    for (let i = 0; i < n; i++) {
      const idx = get_poly_at_pointer()
      if (idx !== -1) split_poly(idx)
    }
  } else if (mode === MODE.CHANGE_POLY_COLOUR_AT_POINTER) {
  } else if (mode === MODE.DELETE_POLY_AT_POINTER) {
    const idx = get_poly_at_pointer()
    if (idx !== -1) polygons.splice(idx, 1)
  }
}

function get_poly_at_pointer() {
  const n_polys = polygons.length
  for (let i = 0; i < n_polys; i++) {
    const poly = polygons[i]
    if (pt_inside_poly(poly.verts, DIM*px_pct, DIM*py_pct)) return i
  }
  return -1
}

function pt_inside_poly(verts, x, y) {
  const n_verts = verts.length;
  let i, j
  let c = false

  for(i = 0, j = n_verts - 1; i < n_verts; j = i++) {
    if(((verts[i].y >= y ) != (verts[j].y >= y)) &&
        (x <= (verts[j].x - verts[i].x) * (y - verts[i].y) / (verts[j].y - verts[i].y) + verts[i].x)
      )
        c = !c
  }
  return c
}

function split_n_polys() {
  const n = settings.action_speed.get_value()
  for (let i = 0; i < n; i++) {
    split_poly(-1)
  }
}

// UI STUFF ===================================================================

function init_control_panel() {
  settings = {
    mode: mk_radio_param("mode", [
      { value: MODE.DEFAULT, label: "Mouse/touch position doesn't do anything special" },
      { value: MODE.CONTROL_SETTINGS, label: "Control settings with mouse/touch position" },
      { value: MODE.SPLIT_POLY_AT_POINTER, label: "Split poly at mouse/touch position" },
      { value: MODE.CHANGE_POLY_COLOUR_AT_POINTER, label: "Change poly colour at mouse/touch position" },
      { value: MODE.DELETE_POLY_AT_POINTER, label: "Delete poly at mouse/touch position" },
    ], MODE_MIN),
    ratio: mk_number_param({
      min: RATIO_MIN,
      max: RATIO_MAX,
      initial_val: RATIO_INITIAL_VAL,
      range: RATIO_RANGE,
      display_scale: RATIO_DISPLAY_SCALE,
    }),
    hue_delta: mk_number_param({
      min: HUE_DELTA_MIN,
      max: HUE_DELTA_MAX,
      initial_val: HUE_DELTA_INITIAL_VAL,
      range: HUE_DELTA_RANGE,
      display_scale: HUE_DELTA_DISPLAY_SCALE,
      invert_btn: true,
    }),
    hue_delta_variance: mk_number_param({
      min: HUE_DELTA_VARIANCE_MIN,
      max: HUE_DELTA_VARIANCE_MAX,
      initial_val: HUE_DELTA_VARIANCE_INITIAL_VAL,
      range: HUE_DELTA_VARIANCE_RANGE,
      display_scale: HUE_DELTA_VARIANCE_DISPLAY_SCALE,
    }),
    lightness_delta: mk_number_param({
      min: LIGHTNESS_DELTA_MIN,
      max: LIGHTNESS_DELTA_MAX,
      initial_val: LIGHTNESS_DELTA_INITIAL_VAL,
      range: LIGHTNESS_DELTA_RANGE,
      display_scale: LIGHTNESS_DELTA_DISPLAY_SCALE,
      variance_sliders: true,
      invert_btn: true,
    }),
    lightness_delta_variance: mk_number_param({
      min: LIGHTNESS_DELTA_VARIANCE_MIN,
      max: LIGHTNESS_DELTA_VARIANCE_MAX,
      initial_val: LIGHTNESS_DELTA_VARIANCE_INITIAL_VAL,
      range: LIGHTNESS_DELTA_VARIANCE_RANGE,
      display_scale: LIGHTNESS_DELTA_VARIANCE_DISPLAY_SCALE,
    }),
    action_speed: mk_number_param({
      min: ACTION_SPEED_MIN,
      max: ACTION_SPEED_MAX,
      initial_val: ACTION_SPEED_INITIAL_VAL,
      range: ACTION_SPEED_RANGE,
      display_scale: ACTION_SPEED_DISPLAY_SCALE,
      variance_sliders: true,
    }),
    draw_debug_lines: mk_toggle_param("Draw debug lines", false)
  }

  const mode_section = mk_control_panel_section("Mode")
  control_panel_el.appendChild(mode_section.el)
  mode_section.content_el.appendChild(settings.mode.el)

  const ratio_section = mk_control_panel_section("Split ratio")
  control_panel_el.appendChild(ratio_section.el)
  ratio_section.content_el.appendChild(settings.ratio.el)

  const hue_delta_section = mk_control_panel_section("Hue delta")
  control_panel_el.appendChild(hue_delta_section.el)
  hue_delta_section.content_el.appendChild(settings.hue_delta.el)

  const hue_delta_variance_section = mk_control_panel_section("Hue delta variance")
  control_panel_el.appendChild(hue_delta_variance_section.el)
  hue_delta_variance_section.content_el.appendChild(settings.hue_delta_variance.el)

  const lightness_delta_section = mk_control_panel_section("Lightness delta")
  control_panel_el.appendChild(lightness_delta_section.el)
  lightness_delta_section.content_el.appendChild(settings.lightness_delta.el)

  const lightness_delta_variance_section = mk_control_panel_section("Lightness delta variance")
  control_panel_el.appendChild(lightness_delta_variance_section.el)
  lightness_delta_variance_section.content_el.appendChild(settings.lightness_delta_variance.el)

  const action_speed_section = mk_control_panel_section("Action speed")
  control_panel_el.appendChild(action_speed_section.el)
  action_speed_section.content_el.appendChild(settings.action_speed.el)

  const undo_redo_section = mk_control_panel_section("Undo/redo")
  control_panel_el.appendChild(undo_redo_section.el)
  undo_redo_section.content_el.appendChild(div_arr([
    mk_button_el("Undo", undo),
    mk_button_el("Redo", redo)
  ]))
  undo_redo_section.content_el.appendChild(div_arr([
    mk_button_el("Undo x 10", () => {
      for (let i = 0; i < 10; i++) {
        undo()
      }
    }),
    mk_button_el("Redo x 10", () => {
      for (let i = 0; i < 10; i++) {
        redo()
      }
    })
  ]))
  undo_redo_section.content_el.appendChild(div_arr([
    mk_button_el("Undo at cur speed", () => {
      const n = settings.action_speed.get_value()
      for (let i = 0; i < n; i++) {
        undo()
      }
    }),
    mk_button_el("Redo at cur speed", () => {
      const n = settings.action_speed.get_value()
      for (let i = 0; i < n; i++) {
        redo()
      }
    })
  ]))
  undo_redo_section.content_el.appendChild(div_arr([
    mk_button_el("Undo all", () => {
      const n = history.length
      for (let i = 0; i < n; i++) {
        undo()
      }
    }),
    mk_button_el("Redo all", () => {
      const n = (history.length - 1) - last_executed_command_idx
      for (let i = 0; i < n; i++) {
        redo()
      }
    })
  ]))

  const misc_section = mk_control_panel_section("Misc")
  control_panel_el.appendChild(misc_section.el)
  misc_section.content_el.appendChild(mk_button_el("Reset canvas", reset_canvas))
  misc_section.content_el.appendChild(settings.draw_debug_lines.el)
}

function mk_control_panel_section(label) {
  const parent_el = document.createElement("div")
  parent_el.className = "control_panel_section"

  const label_el = document.createElement("h4")
  label_el.innerHTML = label
  parent_el.appendChild(label_el)

  const content_el = document.createElement("div")
  content_el.className = "control_panel_section_content"
  parent_el.appendChild(content_el)

  return {
    el: parent_el,
    content_el: content_el
  }
}

function mk_number_param(opts) {
  const parent_el = document.createElement("div")

  const display_scale = opts.display_scale || 1
  const initial_val = opts.initial_val * display_scale
  const min = opts.min * display_scale
  const max = opts.max * display_scale

  for (key in opts.range) {
    opts.range[key] = opts.range[key].map((v) => v * display_scale)
  }

  const slider_el = document.createElement("div")
  noUiSlider.create(slider_el, {
    start: initial_val,
    range: opts.range,
    behaviour: "none",
  })
  parent_el.appendChild(slider_el)

  const number_input_el = mk_number_input_el(min, max)
  parent_el.appendChild(number_input_el)

  function get_display_value() {
    return parseFloat(slider_el.noUiSlider.get())
  }
  function onchange(v, from_slider) {
    if (!from_slider) slider_el.noUiSlider.set(v)
    number_input_el.value = v
  }
  slider_el.noUiSlider.on("update", () => {
    onchange(get_display_value(), true)
  })
  number_input_el.oninput = () => onchange(number_input_el.value, false)
  number_input_el.onchange = () => onchange(number_input_el.value, false)

  if (opts.invert_btn) {
    const invert_btn_el = div(mk_button_el("Invert", () => onchange(get_display_value() * -1, false)))
    parent_el.appendChild(invert_btn_el)
  }

  const ret = {
    get_value: () => { return get_display_value() / display_scale },
    set_value: (v) => onchange(v*display_scale, false),
    el: parent_el
  }

  return ret
}

// options is an array of { value, label }
function mk_radio_param(id, options, initial_val) {
  let val = initial_val

  const parent_el = document.createElement("div")

  function onchange(v) {
    val = parseInt(v)
    for (key in options_map) {
      options_map[key].checked = (key == val)
    }
  }

  const options_map = {}
  for (let i = 0; i < options.length; i++) {
    const opt = options[i]

    const opt_el = document.createElement("div")
    parent_el.appendChild(opt_el)

    const opt_parent_label_el = document.createElement("label")
    opt_el.appendChild(opt_parent_label_el)

    const opt_input_el = document.createElement("input")
    opt_input_el.type = "radio"
    opt_input_el.id = id + "_opt_" + i
    opt_input_el.name = id
    opt_input_el.value = opt.value
    opt_input_el.onchange = () => onchange(opt_input_el.value)
    opt_parent_label_el.appendChild(opt_input_el)

    const opt_label_el = document.createElement("i")
    opt_label_el.innerHTML = opt.label
    opt_parent_label_el.appendChild(opt_label_el)

    options_map[opt.value] = opt_input_el
  }

  onchange(initial_val)

  const ret = {
    get_value: () => val,
    set_value: onchange,
    el: parent_el
  }

  return ret
}

function mk_toggle_param(label, initial_val) {
  let val = initial_val

  const el = document.createElement("label")

  function onchange(v) {
    val = v
    input_el.checked = v
  }

  const input_el = document.createElement("input")
  input_el.type = "checkbox"
  input_el.checked = initial_val
  input_el.onclick = () => onchange(input_el.checked)
  el.appendChild(input_el)

  const label_el = document.createElement("i")
  label_el.innerHTML = label
  el.appendChild(label_el)

  const ret = {
    get_value: () => val,
    set_value: onchange,
    flip: () => onchange(!val),
    el: el
  }

  return ret
}

function mk_button_el(label, onclick) {
  const el = document.createElement("button")
  el.innerHTML = label
  el.onclick = onclick
  return el
}

function mk_number_input_el(min, max) {
  const el = document.createElement("input")
  el.style["min-width"] = "8em"
  el.type = "number"
  el.min = min
  el.max = max
  return el
}

function div(child) {
  const el = document.createElement("div")
  el.appendChild(child)
  return el
}

function div_arr(children) {
  const el = document.createElement("div")
  for (c of children) {
    el.appendChild(c)
  }
  return el
}

// INTERACTION HANDLERS =======================================================

function init_listeners() {
  document.addEventListener("keydown", function(e) {
    if (e.code === "Space") {
      do_action()
    } else if (e.code === "KeyR") {
      reset_canvas()
    } else if (e.code === "KeyD") {
      settings.draw_debug_lines.flip()
    } else if (e.code === "ArrowDown") {
      const n = settings.mode.get_value()
      settings.mode.set_value(Math.min(n+1, MODE_MAX))
    } else if (e.code === "ArrowUp") {
      const n = settings.mode.get_value()
      settings.mode.set_value(Math.max(n-1, MODE_MIN))
    } else if (e.code === "ArrowLeft") {
      const n = settings.action_speed.get_value()
      settings.action_speed.set_value(Math.max(n-1, ACTION_SPEED_MIN))
    } else if (e.code === "ArrowRight") {
      const n = settings.action_speed.get_value()
      settings.action_speed.set_value(Math.min(n+1, ACTION_SPEED_MAX))
    } else if (e.code === "BracketLeft") {
      const n = settings.action_speed.get_value()
      for (let i = 0; i < n; i++) {
        undo()
      }
    } else if (e.code === "BracketRight") {
      const n = settings.action_speed.get_value()
      for (let i = 0; i < n; i++) {
        redo()
      }
    }
  })

  canvas_container_el.addEventListener("mousedown", function(e) {
    handle_pointer_down(e.clientX, e.clientY)
  }, true)
  canvas_container_el.addEventListener("mousemove", function(e) {
    handle_pointer_move(e.clientX, e.clientY)
  }, true)
  canvas_container_el.addEventListener("mouseup", function(e) {
    handle_pointer_up(e.clientX, e.clientY)
  }, true)
  canvas_container_el.addEventListener("touchstart", function(e) {
    if (e.changedTouches.length === 0) return
    handle_pointer_down(e.changedTouches[0].clientX, e.changedTouches[0].clientY)
  }, true)
  canvas_container_el.addEventListener("touchmove", function(e) {
    if (e.changedTouches.length === 0) return
    handle_pointer_move(e.changedTouches[0].clientX, e.changedTouches[0].clientY)
  }, true)
  canvas_container_el.addEventListener("touchend", function(e) {
    if (e.changedTouches.length === 0) return
    handle_pointer_up(e.changedTouches[0].clientX, e.changedTouches[0].clientY)
  }, true)
  window.onresize = resize
}

function handle_pointer_down(x, y) {
  pointer_is_down = true
  update_settings_based_on_pointer_pos(x, y)
}

function handle_pointer_move(x, y) {
  update_settings_based_on_pointer_pos(x, y)
}

function handle_pointer_up(x, y) {
  pointer_is_down = false
  update_settings_based_on_pointer_pos(x, y)
}

function update_settings_based_on_pointer_pos(x, y) {
    const rect = app.view.getBoundingClientRect()
    const canvas_screen_dim = (rect.width/DIM)*DIM
    px_pct = clamp(0, 1, (x - rect.x)/canvas_screen_dim)
    py_pct = clamp(0, 1, (y - rect.y)/canvas_screen_dim)
    if (settings.mode.get_value() == MODE.CONTROL_SETTINGS) {
      settings.ratio.set_value(lerp(RATIO_MIN, RATIO_MAX, 1 - py_pct))
      settings.lightness_delta.set_value(lerp(LIGHTNESS_DELTA_MIN, LIGHTNESS_DELTA_MAX, px_pct))
    }
}

// UNDO/REDO ==================================================================

function undo() {
  if (last_executed_command_idx === -1) return
  const command = history[last_executed_command_idx]

  lines.splice(lines.length - 1, 1)

  // poly was deleted, then p1 was added, then p2 was added. need to do this in reverse
  polygons.splice(command.p2_idx, 1)
  polygons.splice(command.p1_idx, 1)
  polygons.splice(command.poly_idx, 0, command.poly)

  last_executed_command_idx--
}

function redo() {
  if (last_executed_command_idx === history.length - 1) return
  const command = history[last_executed_command_idx + 1]

  lines.push(command.line)

  // poly was deleted, then p1 was added, then p2 was added. need to do this in the same order
  polygons.splice(command.poly_idx, 1)
  polygons.splice(command.p1_idx, 0, command.p1)
  polygons.splice(command.p2_idx, 0, command.p2)

  last_executed_command_idx++
}

// SPLIT POLY =================================================================

// if poly_idx is out of range, ignores it and uses the largest poly.
function split_poly(poly_idx) {
  const n_polys = polygons.length
  if (n_polys === 0) return
  if (poly_idx < 0 || poly_idx >= n_polys) poly_idx = n_polys - 1 // largest one
  const poly = polygons[poly_idx]

  // u, v <- two random points along circumference iterated a bunch of times, keep the two with the smallest euclidean distance
  let smallest_dist = Number.MAX_SAFE_INTEGER
  let found = false
  let uvert = pt(0, 0)
  let vvert = pt(0, 0)
  let uidx
  let vidx
  let tmpuvert = pt(0, 0)
  let tmpvvert = pt(0, 0)
  let ratio = settings.ratio.get_value()
  for (let i = 0; i < 10; i++) {
    const pct_u = Math.random()
    const pct_v = pct_u+0.5*ratio
    tmpuidx = calc_vert_at_pct_along_perimeter(poly, pct_u, tmpuvert)
    tmpvidx = calc_vert_at_pct_along_perimeter(poly, pct_v, tmpvvert)
    if (tmpuidx == tmpvidx) continue
    const dist = distance_squared(tmpuvert, tmpvvert)
    if (dist < smallest_dist) {
      found = true
      smallest_dist = dist
      uvert.x = tmpuvert.x
      uvert.y = tmpuvert.y
      vvert.x = tmpvvert.x
      vvert.y = tmpvvert.y
      uidx = tmpuidx
      vidx = tmpvidx
    }
  }
  if (!found) return

  const line = mk_line(uvert, vvert)
  lines.push(line)
  const n_verts = poly.verts.length

  // init poly 1 to be u, then add all the verts up to but not including v.idx, then add v
  const p1_verts = [uvert]
  let i = uidx
  while (true) {
    const j = i % n_verts
    if (j === vidx) {
      p1_verts.push(vvert)
      break
    }
    p1_verts.push(poly.verts[j])
    i++
  }

  // init poly 2 to be v, then add all the verts up to but not including u.idx, then add u
  const p2_verts = [vvert]
  i = vidx
  while (true) {
    const j = i % n_verts
    if (j === uidx) {
      p2_verts.push(uvert)
      break
    }
    p2_verts.push(poly.verts[j])
    i++
  }

  const hue_delta = settings.hue_delta.get_value()
  const hue_delta_variance = settings.hue_delta_variance.get_value()
  const hdv1 = hue_delta_variance*lerp(-1, 1, Math.random())
  const hdv2 = hue_delta_variance*lerp(-1, 1, Math.random())
  const h1 = (poly.h + hue_delta + hdv1 + 1)%1
  const h2 = (poly.h + hue_delta + hdv2 + 1)%1

  const s = 1

  const lightness_delta = settings.lightness_delta.get_value()
  const lightness_delta_variance = settings.lightness_delta_variance.get_value()
  const ldv1 = lightness_delta_variance*lerp(-1, 1, Math.random())
  const ldv2 = lightness_delta_variance*lerp(-1, 1, Math.random())
  const l1 = clamp(0, 1, poly.l + lightness_delta + ldv1)
  const l2 = clamp(0, 1, poly.l + lightness_delta + ldv2)

  const p1 = mk_poly(p1_verts, h1, s, l1)
  const p2 = mk_poly(p2_verts, h2, s, l2)
  polygons.splice(poly_idx, 1)
  const p1_idx = insert_poly(p1)
  const p2_idx = insert_poly(p2)

  const command = {
    poly_idx: poly_idx,
    poly: poly,
    p1_idx: p1_idx,
    p1: p1,
    p2_idx: p2_idx,
    p2: p2,
    line: line
  }
  if (last_executed_command_idx === history.length - 1) { // at the head, add the command
    history.push(command)
  } else { // not at the head, need to throw away the commands after the current pos before adding the command
    history = history.slice(0, last_executed_command_idx + 1)
    history.push(command)
  }
  last_executed_command_idx++
}

// returns the insertion index
function insert_poly(poly) {
  const i = sorted_poly_idx(poly)
  polygons.splice(i, 0, poly)
  return i
}

function sorted_poly_idx(poly) {
    const c = poly.circumference
    let m = 0
    let n = polygons.length - 1

    while (m <= n) {
        const k = (m + n) >> 1;
        const cmp = c - polygons[k].circumference;
        if (cmp > 0) m = k + 1;
        else if (cmp < 0) n = k - 1;
        else return k;
    }

    return (m == 0) ? 0 : m - 1;
}

function calc_vert_at_pct_along_perimeter(poly, pct, out) {
  let dist_remaining = poly.circumference * (pct % 1)
  const n = poly.verts.length
  for (let i = 1; i <= n; i++) {
    const j = i % n
    const dist_to_vert_j = poly.line_length_to_idx[j]
    if (dist_remaining > dist_to_vert_j) {
      // the desired vert lies after j
      dist_remaining -= dist_to_vert_j
    } else {
      // the desired vert lies between j and j-1
      const pct_along_cur_line = dist_remaining / dist_to_vert_j
      const v0 = poly.verts[(j-1+n)%n]
      const v1 = poly.verts[j]
      out.x = lerp(v0.x, v1.x, pct_along_cur_line)
      out.y = lerp(v0.y, v1.y, pct_along_cur_line)
      return j
    }
  }
  return -1
}

function mk_line(u, v) {
  return {
    u: u,
    v: v
  }
}

function mk_poly(verts, h, s, l) {
  const ret = {
    circumference: 0,
    verts: verts,
    line_length_to_idx: [],
    h: h,
    s: s,
    l: l,
    hex: hsl_to_hex(h, s, l)
  }

  const n = verts.length
  for (let i = 1; i <= n; i++) {
    const v0 = verts[i-1]
    const v1 = verts[i%n]
    const dist = distance_squared(v0, v1)
    ret.circumference += dist
    if (i < n) ret.line_length_to_idx.push(dist)
    else ret.line_length_to_idx.splice(0, 0, dist)
  }

  return ret
}

function pt(x, y) {
  return new PIXI.Point(x, y)
}

// MATH HELPERS ===============================================================

function clamp(min, max, n) {
  return Math.min(Math.max(n, min), max)
}

function lerp(a, b, t) {
  return a*(1-t)+b*t
}

function distance_squared(v0, v1) {
  const a = v1.x - v0.x
  const b = v1.y - v0.y
  return a*a + b*b
}

// HSL TO HEX =================================================================

// hsl logic based on
// https://stackoverflow.com/questions/36721830/convert-hsl-to-rgb-and-hex

// h,s,l in [0, 1]
function hsl_to_hex(h, s, l) {
  let r, g, b
  if (s === 0) {
    r = g = b = l // achromatic
  } else {
    const q = l < 0.5 ? l * (1 + s) : l + s - l * s
    const p = 2 * l - q
    r = hue_to_rgb(p, q, h + 1/3)
    g = hue_to_rgb(p, q, h)
    b = hue_to_rgb(p, q, h - 1/3)
  }
  return (Math.round(r*255)<<16) + (Math.round(g*255)<<8) + Math.round(b*255)
}

function hue_to_rgb(p, q, t) {
  if (t < 0) t += 1
  if (t > 1) t -= 1
  if (t < 1 / 6) return p + (q - p) * 6 * t
  if (t < 1 / 2) return q
  if (t < 2 / 3) return p + (q - p) * (2 / 3 - t) * 6
  return p
}

// ============================================================================

main()
