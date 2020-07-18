// TODOS ======================================================================

// TODO kbd shortcuts
// TODO have radio button choice for which property is controlled by x/y
// TODO change poly colour based on cur settings
// TODO set poly colour
// TODO save presets
// TODO delete poly
// TODO undo/redo?
// TODO save replay
// TODO mode to make split with pointer start/end pos

// CONSTANTS ==================================================================

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
const RATIO_MIN = 0.05
const RATIO_MAX = 1
const RATIO_STEP = 0.01
const HUE_DELTA_MIN = -0.1
const HUE_DELTA_MAX = 0.1
const HUE_DELTA_STEP = 0.001
const LIGHTNESS_DELTA_MIN = -0.01
const LIGHTNESS_DELTA_MAX = 0.01
const LIGHTNESS_DELTA_STEP = 0.0001
const N_SPLITS_PER_TICK_MIN = 1
const N_SPLITS_PER_TICK_MAX = 50
const N_SPLITS_PER_TICK_STEP = 1

// VARIABLES ==================================================================

let resolution
let app
let canvas_container_el
let control_panel_el
let pointer_is_down // true if mouse/touch is down
let px_pct // pointer x as frac of DIM
let py_pct // pointer y as frac of DIM
let g // graphics
let polygons
let lines
let settings

// SETUP ======================================================================

function main() {
  resolution = 1*(window.devicePixelRatio || 1)
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
  let rect = canvas_container_el.getBoundingClientRect()

  if (window_ratio < 0.8) { // very tall
    document.body.style["flex-direction"] = "column"
    canvas_container_el.style["flex-basis"] = Math.min(w, DIM*resolution) + "px"
  } else if (window_ratio > 1.5) { // very wide
    document.body.style["flex-direction"] = "row"
    canvas_container_el.style["flex-basis"] = Math.min(h, DIM*resolution) + "px"
  } else { // roughly square
    document.body.style["flex-direction"] = "row"
    canvas_container_el.style["flex-basis"] = "65%"
  }

  rect = canvas_container_el.getBoundingClientRect() // recalc
  const canvas_len = 0.9*Math.min(rect.width, rect.height) + "px"
  app.view.style["max-width"] = canvas_len
  app.view.style["max-height"] = canvas_len
}

function reset() {
  settings.mode.set_value(MODE.SPLIT_POLY_AT_POINTER)
  settings.ratio.set_value(lerp(RATIO_MIN, RATIO_MAX, 0.5))
  settings.hue_delta.set_value(lerp(HUE_DELTA_MIN, HUE_DELTA_MAX, 0.5))
  settings.lightness_delta.set_value(lerp(LIGHTNESS_DELTA_MIN, LIGHTNESS_DELTA_MAX, 0.75))
  settings.n_splits_per_tick.set_value(N_SPLITS_PER_TICK_MIN)
  settings.draw_debug_lines.set_value(false)

  reset_canvas()
}

function reset_canvas() {
  const p00 = pt(0, 0)
  const p10 = pt(DIM, 0)
  const p11 = pt(DIM, DIM)
  const p01 = pt(0, DIM)

  lines = [mk_line(p00, p10), mk_line(p10, p11), mk_line(p11, p01), mk_line(p01, p00)]

  const first_poly = mk_poly([p00, p10, p11, p01], 0.5, 0.8, 0.2)
  polygons = [first_poly]
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
    const mode = settings.mode.get_value()
    if (mode === MODE.DEFAULT) {
      split_n_polys()
    } else if (mode === MODE.CONTROL_SETTINGS) {
      split_n_polys()
    } else if (mode === MODE.SPLIT_POLY_AT_POINTER) {
      const n = settings.n_splits_per_tick.get_value()
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
  const n = settings.n_splits_per_tick.get_value()
  for (let i = 0; i < n; i++) {
    split_poly(-1)
  }
}

// UI STUFF ===================================================================

function init_control_panel() {
  settings = {
    mode: mk_radio_param("Mode:", [
      { value: MODE.DEFAULT, label: "Mouse/touch position doesn't do anything special" },
      { value: MODE.CONTROL_SETTINGS, label: "Control settings with mouse/touch position" },
      { value: MODE.SPLIT_POLY_AT_POINTER, label: "Split poly at mouse/touch position" },
      { value: MODE.CHANGE_POLY_COLOUR_AT_POINTER, label: "Change poly colour at mouse/touch position" },
      { value: MODE.DELETE_POLY_AT_POINTER, label: "Delete poly at mouse/touch position" },
    ], MODE.DEFAULT),
    ratio: mk_number_param("Split ratio:", RATIO_MIN, RATIO_MAX, RATIO_MAX, RATIO_STEP),
    hue_delta: mk_number_param("Hue delta:", HUE_DELTA_MIN, HUE_DELTA_MAX, HUE_DELTA_MAX, HUE_DELTA_STEP),
    lightness_delta: mk_number_param("Lightness delta:", LIGHTNESS_DELTA_MIN, LIGHTNESS_DELTA_MAX, LIGHTNESS_DELTA_MAX, LIGHTNESS_DELTA_STEP),
    n_splits_per_tick: mk_number_param("# splits per tick:", N_SPLITS_PER_TICK_MIN, N_SPLITS_PER_TICK_MAX, N_SPLITS_PER_TICK_MIN, N_SPLITS_PER_TICK_STEP),
    draw_debug_lines: mk_toggle_param("Draw debug lines", false)
  }

  control_panel_el.appendChild(settings.mode.el)

  control_panel_el.appendChild(settings.ratio.el)

  control_panel_el.appendChild(settings.hue_delta.el)
  settings.hue_delta.el.appendChild(mk_button_el("Invert", () => settings.hue_delta.set_value(-1 * settings.hue_delta.get_value())))

  control_panel_el.appendChild(settings.lightness_delta.el)
  settings.lightness_delta.el.appendChild(mk_button_el("Invert", () => settings.lightness_delta.set_value(-1 * settings.lightness_delta.get_value())))

  control_panel_el.appendChild(settings.n_splits_per_tick.el)

  control_panel_el.appendChild(mk_button_el("Reset canvas", reset_canvas))

  control_panel_el.appendChild(settings.draw_debug_lines.el)
}

function mk_number_param(label, min, max, initial_val, step_size) {
  let val = initial_val

  const parent_el = document.createElement("div")

  const label_el = document.createElement("h4")
  label_el.innerHTML = label
  parent_el.appendChild(label_el)

  const asdf = document.createElement("div")
  parent_el.appendChild(asdf)

  const slider_el = mk_slider_el(min, max, step_size)
  asdf.appendChild(slider_el)

  const number_input_el = mk_number_input_el(min, max, step_size)
  asdf.appendChild(number_input_el)

  function onchange(v) {
    val = parseFloat(v)
    slider_el.value = v
    number_input_el.value = v
  }
  slider_el.oninput = () => onchange(slider_el.value)
  slider_el.onchange = () => onchange(slider_el.value)
  number_input_el.oninput = () => onchange(number_input_el.value)
  number_input_el.onchange = () => onchange(number_input_el.value)

  const ret = {
    get_value: () => val,
    set_value: onchange,
    el: parent_el
  }
  return ret
}

// options is an array of { value, label }
function mk_radio_param(label, options, initial_val) {
  let val = initial_val

  const parent_el = document.createElement("div")

  const label_el = document.createElement("h4")
  label_el.innerHTML = label
  parent_el.appendChild(label_el)

  const options_container_el = document.createElement("div")
  parent_el.appendChild(options_container_el)

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
    options_container_el.appendChild(opt_el)

    const opt_parent_label_el = document.createElement("label")
    opt_el.appendChild(opt_parent_label_el)

    const opt_input_el = document.createElement("input")
    opt_input_el.type = "radio"
    opt_input_el.id = label + "_opt_" + i
    opt_input_el.name = label
    opt_input_el.value = opt.value
    opt_input_el.onchange = () => onchange(opt_input_el.value)
    opt_parent_label_el.appendChild(opt_input_el)

    const opt_label_el = document.createElement("i")
    opt_label_el.innerHTML = opt.label
    opt_parent_label_el.appendChild(opt_label_el)

    options_map[opt.value] = opt_input_el
  }

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

function mk_slider_el(min, max, step_size) {
  const el = document.createElement("input")
  el.type = "range"
  el.class = "slider"
  el.min = min
  el.max = max
  el.step = step_size
  return el
}

function mk_number_input_el(min, max, step_size) {
  const el = document.createElement("input")
  el.type = "number"
  el.min = min
  el.max = max
  el.step = step_size
  return el
}

// INTERACTION HANDLERS =======================================================

function init_listeners() {
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

  lines.push(mk_line(uvert, vvert))
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

  const h1 = (poly.h + settings.hue_delta.get_value() + 1)%1
  const h2 = (poly.h + settings.hue_delta.get_value() + 1)%1
  const s = 1
  const l = clamp(0, 1, poly.l + settings.lightness_delta.get_value())
  const p1 = mk_poly(p1_verts, h1, s, l)
  const p2 = mk_poly(p2_verts, h2, s, l)
  polygons.splice(poly_idx, 1)
  insert_poly(p1)
  insert_poly(p2)
}

function insert_poly(poly) {
  const i = sorted_poly_idx(poly)
  polygons.splice(i, 0, poly)
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

const trace = console.log

// ============================================================================

main()
