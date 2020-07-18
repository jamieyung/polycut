// TODO have radio button choice for which property is controlled by x/y
// TODO control poly
// TODO change poly colour based on cur settings
// TODO set poly colour
// TODO save presets
// TODO delete poly
// TODO undo/redo?
// TODO save replay
// TODO col delta
// TODO brightness delta
// TODO mode to split poly under pointer
// TODO mode to make split with pointer start/end pos

// constants
const DIM = 800
const DIM_2 = DIM/2
const TWO_PI = Math.PI*2
const MODE = {
  CONTROL_SETTINGS: 0,
  CONTROL_POLY: 1,
  CHANGE_POLY_COLOUR: 2,
  DELETE_POLY: 3,
}
const RATIO_MIN = 0.05
const RATIO_MAX = 1
const LIGHTNESS_DELTA_MIN = -0.01
const LIGHTNESS_DELTA_MAX = 0.01
const N_SPLITS_PER_TICK_MIN = 1
const N_SPLITS_PER_TICK_MAX = 50

let app
let canvas_container_el
let control_panel_el
let pointer_is_down
let px // pointer x relative to top left corner of canvas
let py // pointer y relative to top left corner of canvas
let px_pct // pointer x as frac of DIM
let py_pct // pointer y as frac of DIM
let g // graphics
let polygons

let settings

// SETUP ======================================================================

function main() {
  app = new PIXI.Application({
    width: DIM,
    height: DIM,
    backgroundColor: 0xffffff,
    resolution: 1*(window.devicePixelRatio || 1),
    antialias: true
  })
  canvas_container_el = document.getElementById("canvas_container")
  canvas_container_el.appendChild(app.view)
  control_panel_el = document.getElementById("control_panel")

  init_listeners()

  pointer_is_down = false

  px = 0
  py = 0
  px_pct = 0
  py_pct = 0

  g = new PIXI.Graphics()
  app.stage.addChild(g)

  polygons = []

  init_settings()

  resize()
  reset()

  app.ticker.add(tick)
}

function resize() {
  const rect = canvas_container_el.getBoundingClientRect()
  const canvas_len = 0.9*Math.min(rect.width, rect.height) + "px"
  app.view.style["max-width"] = canvas_len
  app.view.style["max-height"] = canvas_len
}

function reset() {
  settings.mode = MODE.CONTROL_SETTINGS
  settings.ratio.set_value(lerp(RATIO_MIN, RATIO_MAX, 0.5))
  settings.lightness_delta.set_value(lerp(LIGHTNESS_DELTA_MIN, LIGHTNESS_DELTA_MAX, 0.5))
  settings.n_splits_per_tick.set_value(N_SPLITS_PER_TICK_MIN)

  const first_poly = mk_poly([pt(0, 0), pt(DIM, 0), pt(DIM, DIM), pt(0, DIM)], 0.5, 0.76, 0.7)
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

  if (pointer_is_down) {
    for (let i = 0; i < settings.n_splits_per_tick.get_value(); i++) {
      split_poly(-1)
    }
    g.beginFill(0)
    g.drawRect(px_pct*DIM, py_pct*DIM, 20, 20)
    g.endFill()
  }
}

// UI STUFF ===================================================================

function init_settings() {
  settings = {
    mode: MODE.CONTROL_SETTINGS,
    ratio: mk_number_param("Split ratio:", RATIO_MIN, RATIO_MAX, RATIO_MAX, 0.05),
    lightness_delta: mk_number_param("Lightness delta:", LIGHTNESS_DELTA_MIN, LIGHTNESS_DELTA_MAX, LIGHTNESS_DELTA_MAX, 0.0001),
    n_splits_per_tick: mk_number_param("# splits per tick:", N_SPLITS_PER_TICK_MIN, N_SPLITS_PER_TICK_MAX, N_SPLITS_PER_TICK_MIN, 1)
  }
}

function mk_number_param(label, min, max, initial_val, step_size) {
  let val = initial_val

  const parent_el = document.createElement("div")
  control_panel_el.appendChild(parent_el)

  const label_el = document.createElement("b")
  label_el.innerHTML = label
  parent_el.appendChild(label_el)

  const slider_el = mk_slider_el(min, max, step_size)
  parent_el.appendChild(slider_el)

  const number_input_el = mk_number_input_el(min, max, step_size)
  parent_el.appendChild(number_input_el)

  function onchange (v) {
    val = v
    slider_el.value = v
    number_input_el.value = v
  }
  slider_el.oninput = () => onchange(slider_el.value)
  slider_el.onchange = () => onchange(slider_el.value)
  number_input_el.oninput = () => onchange(number_input_el.value)
  number_input_el.onchange = () => onchange(number_input_el.value)

  const ret = {
    get_value: () => {
      return val
    },
    set_value: (v) => {
      val = v
      slider_el.value = v
      number_input_el.value = v
    }
  }
  return ret
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
    px = x - rect.x
    py = y - rect.y
    px_pct = clamp(0, 1, px/canvas_screen_dim)
    py_pct = clamp(0, 1, py/canvas_screen_dim)
    if (settings.mode == MODE.CONTROL_SETTINGS) {
      settings.ratio.set_value(lerp(RATIO_MIN, RATIO_MAX, 1 - py_pct))
      settings.lightness_delta.set_value(lerp(LIGHTNESS_DELTA_MIN, LIGHTNESS_DELTA_MAX, px_pct))
    }
}

// SPLIT POLY =================================================================

// if poly_idx is out of range, ignores it and uses the largest poly.
function split_poly(poly_idx) {
  n_polys = polygons.length
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

  // TODO color
  const s = 1
  const l = clamp(0, 1, poly.l + settings.lightness_delta.get_value())
  const p1 = mk_poly(p1_verts, Math.random(), s, l)
  const p2 = mk_poly(p2_verts, Math.random(), s, l)
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
