// CONSTANTS ==================================================================

const trace = console.log

const DIM = 800

const INTERACTION_MODE_CUT_POLY_AT_POINTER = 1
const INTERACTION_MODE_CUT_LARGEST_POLY = 2
const INTERACTION_MODE_DELETE_POLY_AT_POINTER = 3
const INTERACTION_MODE_CHANGE_POLY_COLOUR_AT_POINTER = 4
const INTERACTION_MODE_MANUAL_CUT = 5

const FLASH_LINE_INITIAL_TICKS_LEFT = 30

const MANUAL_CUT_STATE_WAITING_FOR_PT1 = 1
const MANUAL_CUT_STATE_WAITING_FOR_PT2 = 2

// VARIABLES ==================================================================

let hsl_to_hex

let resolution
let app
let polygons_container
let flash_lines_graphics
let debug_lines_container
let pointer_crosshair_graphics
let manual_cut_line_graphics
let canvas_container_el
let pointer_is_down
let pointer_pos // relative to canvas left
let last_executed_command_idx // if > -1, means at state just after performing history[last_executed_command_idx]
let polygons
let lines
let flash_lines
let history
let interaction_mode
let manual_cut_data
let params

// EXPORTS AND SETUP ==========================================================

exports.get_canvas_container_event_target = function() {
  return document.getElementById("canvas_container")
}

function update_px_py(px, py) {
  const rect = app.view.getBoundingClientRect()
  const canvas_screen_dim = (rect.width/DIM)*DIM
  pointer_pos.x = DIM*clamp(0, 1, (px - rect.x)/canvas_screen_dim)
  pointer_pos.y = DIM*clamp(0, 1, (py - rect.y)/canvas_screen_dim)
}

function point_is_inside_canvas(px, py) {
  const rect = app.view.getBoundingClientRect()
  const canvas_screen_dim = (rect.width/DIM)*DIM
  const x_pct = (px - rect.x)/canvas_screen_dim
  const y_pct = (py - rect.y)/canvas_screen_dim
  return x_pct >= 0 && x_pct <= 1 && y_pct >= 0 && y_pct <= 1
}

exports.pointer_down = function(px, py) {
  return function() {
    pointer_is_down = true
    update_px_py(px, py)
    if (interaction_mode === INTERACTION_MODE_MANUAL_CUT) {
      manual_cut_data.state = MANUAL_CUT_STATE_WAITING_FOR_PT2
      manual_cut_data.pt1.x = pointer_pos.x
      manual_cut_data.pt1.y = pointer_pos.y
    }
  }
}

exports.pointer_move = function(px, py) {
  return function() {
    update_px_py(px, py)
  }
}

exports.pointer_up = function(px, py) {
  return function() {
    pointer_is_down = false
    update_px_py(px, py)
    if (interaction_mode === INTERACTION_MODE_MANUAL_CUT) {
      manual_cut_data.state = MANUAL_CUT_STATE_WAITING_FOR_PT1
      const arr = get_manual_cut_intersection_info()
      for (info of arr) {
        var idx = polygons.indexOf(info.poly)
        if (idx !== -1) perform_cut(info.poly, idx, info.pt1, info.pt2, info.i1, info.i2)
      }
    }
  }
}

exports.set_interaction_mode_cut_poly_at_pointer = function() {
  interaction_mode = INTERACTION_MODE_CUT_POLY_AT_POINTER
}

exports.set_interaction_mode_cut_largest_poly = function() {
  interaction_mode = INTERACTION_MODE_CUT_LARGEST_POLY
}

exports.set_interaction_mode_delete_poly_at_pointer = function() {
  interaction_mode = INTERACTION_MODE_DELETE_POLY_AT_POINTER
}

exports.set_interaction_mode_change_poly_colour_at_pointer = function() {
  interaction_mode = INTERACTION_MODE_CHANGE_POLY_COLOUR_AT_POINTER
}

exports.set_interaction_mode_manual_cut = function() {
  interaction_mode = INTERACTION_MODE_MANUAL_CUT
}

exports.set_params = function(state) {
  return function() {
    params = state.params

    params.draw_debug_lines = state.draw_debug_lines
    debug_lines_container.visible = params.draw_debug_lines

    params.draw_pointer_crosshair = state.draw_pointer_crosshair
  }
}

exports.init = function(args) {
  return function() {
    hsl_to_hex = args.hsl_to_hex

    resolution = 2*(window.devicePixelRatio || 1)
    app = new PIXI.Application({
      width: DIM,
      height: DIM,
      backgroundColor: 0xffffff,
      resolution: resolution,
      antialias: true,
      preserveDrawingBuffer: true
    })

    polygons_container = new PIXI.Container()
    app.stage.addChild(polygons_container)

    flash_lines_graphics = new PIXI.Graphics()
    app.stage.addChild(flash_lines_graphics)

    debug_lines_container = new PIXI.Container()
    app.stage.addChild(debug_lines_container)

    pointer_crosshair_graphics = new PIXI.Graphics()
    app.stage.addChild(pointer_crosshair_graphics)

    manual_cut_line_graphics = new PIXI.Graphics()
    app.stage.addChild(manual_cut_line_graphics)

    canvas_container_el = document.getElementById("canvas_container")
    canvas_container_el.appendChild(app.view)

    pointer_is_down = false

    pointer_pos = new PIXI.Point(0, 0)

    polygons = []

    lines = []

    flash_lines = []

    history = []

    interaction_mode = INTERACTION_MODE_CUT_POLY_AT_POINTER

    manual_cut_data = {
      state: MANUAL_CUT_STATE_WAITING_FOR_PT1,
      pt1: new PIXI.Point(0, 0)
    }

    reset_canvas()

    window.onresize = resize
    resize()

    app.ticker.add(tick)
  }
}

function resize() {
  const w = window.innerWidth
  const h = window.innerHeight
  const window_ratio = w/h

  const root = document.getElementById("root")
  if (window_ratio < 0.8) { // very tall
    root.style["flex-direction"] = "column"
    canvas_container_el.style["width"] = "100%"
    canvas_container_el.style["height"] = w + "px"
  } else if (window_ratio > 1.5) { // very wide
    root.style["flex-direction"] = "row"
    canvas_container_el.style["height"] = "100%"
    canvas_container_el.style["width"] = h + "px"
  } else { // roughly square
    root.style["flex-direction"] = "row"
    canvas_container_el.style["height"] = "100%"
    canvas_container_el.style["width"] = (w*0.65) + "px"
  }

  let rect = canvas_container_el.getBoundingClientRect()
  const canvas_len = 0.9*Math.min(rect.width, rect.height) + "px"
  app.view.style["max-width"] = canvas_len
  app.view.style["max-height"] = canvas_len
}

function reset_canvas() {
  const p00 = pt(0, 0)
  const p10 = pt(DIM, 0)
  const p11 = pt(DIM, DIM)
  const p01 = pt(0, DIM)

  lines = [mk_line(p00, p10), mk_line(p10, p11), mk_line(p11, p01), mk_line(p01, p00)]
  flash_lines = []

  polygons_container.removeChildren()
  const first_poly = mk_poly([p00, p10, p11, p01])
  const h = Math.random()
  const s = Math.random()
  const l = lerp(0.1, 0.6, Math.random())
  set_poly_colour(first_poly, h, s, l)
  polygons_container.addChild(first_poly.graphics)
  polygons = [first_poly]

  debug_lines_container.removeChildren()

  history = []

  last_executed_command_idx = -1
}

exports.reset_canvas = reset_canvas

exports.save_as_png = function() {
  app.view.toBlob((blob) => {
    saveAs(blob, "image.png")
  })
}

exports.history_add_command_listener = function(f) {
  return function(event) {
    return f(event.detail)
  }
}

// TICK =======================================================================

function tick() {
  flash_lines_graphics.clear()
  for (let i = flash_lines.length - 1; i >= 0; i--) {
    const flash_line = flash_lines[i]
    if (flash_line.ticks_left === 0) {
      flash_lines.splice(i, 1)
      continue
    }
    const ratio = flash_line.ticks_left/FLASH_LINE_INITIAL_TICKS_LEFT
    flash_lines_graphics.lineStyle(1, 0xffffff, ratio)
    flash_line.ticks_left--
    const ratio0 = ease_in_quad(ratio)
    const x0 = lerp(flash_line.u.x, flash_line.v.x, ratio0)
    const y0 = lerp(flash_line.u.y, flash_line.v.y, ratio0)
    flash_lines_graphics.moveTo(x0, y0)
    const ratio1 = ease_out_quad(ratio)
    const x1 = lerp(flash_line.u.x, flash_line.v.x, ratio1)
    const y1 = lerp(flash_line.u.y, flash_line.v.y, ratio1)
    flash_lines_graphics.lineTo(x1, y1)
  }

  pointer_crosshair_graphics.clear()
  if (params.draw_pointer_crosshair) {
    if (interaction_mode === INTERACTION_MODE_CUT_POLY_AT_POINTER
      || interaction_mode === INTERACTION_MODE_DELETE_POLY_AT_POINTER
      || interaction_mode === INTERACTION_MODE_CHANGE_POLY_COLOUR_AT_POINTER
      || interaction_mode === INTERACTION_MODE_MANUAL_CUT) {
      pointer_crosshair_graphics.lineStyle(2, 0xff00ff)
      pointer_crosshair_graphics.moveTo(pointer_pos.x, 0)
      pointer_crosshair_graphics.lineTo(pointer_pos.x, DIM)
      pointer_crosshair_graphics.moveTo(0, pointer_pos.y)
      pointer_crosshair_graphics.lineTo(DIM, pointer_pos.y)
    }
  }

  manual_cut_line_graphics.clear()
  if (interaction_mode === INTERACTION_MODE_MANUAL_CUT) {
    if (manual_cut_data.state === MANUAL_CUT_STATE_WAITING_FOR_PT2) {
      manual_cut_line_graphics.lineStyle(1, 0xffffff)
      manual_cut_line_graphics.moveTo(manual_cut_data.pt1.x, manual_cut_data.pt1.y)
      manual_cut_line_graphics.lineTo(pointer_pos.x, pointer_pos.y)
      const arr = get_manual_cut_intersection_info()

      for (info of arr) {
        manual_cut_line_graphics.lineStyle(3, get_overlay_hex_for_poly(info.poly))
        manual_cut_line_graphics.moveTo(info.pt1.x, info.pt1.y)
        manual_cut_line_graphics.lineTo(info.pt2.x, info.pt2.y)
      }
      for (info of arr) {
        manual_cut_line_graphics.lineStyle(3, get_overlay_hex_for_poly(info.poly))
        manual_cut_line_graphics.drawCircle(info.pt1.x, info.pt1.y, 7)
        manual_cut_line_graphics.drawCircle(info.pt2.x, info.pt2.y, 7)
      }
    }
  }

  if (pointer_is_down) handle_pointer_is_down()
}

function get_manual_cut_intersection_info() {
  const ret = []
  const n_polys = polygons.length
  for (let poly_idx = 0; poly_idx < n_polys; poly_idx++) {
    const poly = polygons[poly_idx]
    const n = poly.verts.length
    let found_intersection_pt1 = null
    let found_i = -1
    for (let i = 0; i < n; i++) {
      const a = poly.verts[(i-1+n)%n]
      const b = poly.verts[i]
      const pt = lines_intersection_point(a, b, manual_cut_data.pt1, pointer_pos)
      if (pt === null) continue
      if (found_intersection_pt1 === null) {
        found_intersection_pt1 = pt
        found_i = i
      } else {
        // both intersection points have now been found
        ret.push({
          poly: poly,
          pt1: found_intersection_pt1,
          i1: found_i,
          pt2: pt,
          i2: i
        })
        break
      }
    }
  }
  return ret
}

function get_overlay_hex_for_poly(poly) {
  const l_diff = 0.4
  if (poly.l === 0) {
    return hsl_to_hex(0, 0, l_diff)
  } else if (poly.l === 1) {
    return hsl_to_hex(0, 0, 1-l_diff)
  } else {
    const l = poly.l < 0.5 ? (poly.l + l_diff) : (poly.l - l_diff)
    return hsl_to_hex(poly.h, poly.s, l)
  }
}

function handle_pointer_is_down() {
  if (interaction_mode === INTERACTION_MODE_CUT_POLY_AT_POINTER) {
    const n = params.n_cuts_per_tick
    for (let i = 0; i < n; i++) {
      const idx = get_poly_at(pointer_pos)
      if (idx !== -1) cut_poly(idx)
    }
  } else if (interaction_mode === INTERACTION_MODE_CUT_LARGEST_POLY) {
    const n = params.n_cuts_per_tick
    for (let i = 0; i < n; i++) {
      cut_poly(polygons.length - 1) // largest one
    }
  } else if (interaction_mode === INTERACTION_MODE_DELETE_POLY_AT_POINTER) {
      const idx = get_poly_at(pointer_pos)
      if (idx !== -1) {
        delete_poly_at(idx)
      }
  } else if (interaction_mode == INTERACTION_MODE_CHANGE_POLY_COLOUR_AT_POINTER) {
      const idx = get_poly_at(pointer_pos)
      if (idx !== -1) {
        const poly = polygons[idx]
        set_child_colour_based_on_parent_colour(poly, poly)
      }
  }
}

function get_poly_at(pt) {
  const n_polys = polygons.length
  for (let i = 0; i < n_polys; i++) {
    const poly = polygons[i]
    if (pt_inside_poly(poly.verts, pt)) return i
  }
  return -1
}

function pt_inside_poly(verts, pt) {
  const n_verts = verts.length;
  let i, j
  let c = false

  for(i = 0, j = n_verts - 1; i < n_verts; j = i++) {
    if(((verts[i].y >= pt.y ) != (verts[j].y >= pt.y)) &&
        (pt.x <= (verts[j].x - verts[i].x) * (pt.y - verts[i].y) / (verts[j].y - verts[i].y) + verts[i].x)
      )
        c = !c
  }
  return c
}

function delete_poly_at(idx) {
    const poly = polygons[idx]
    if (!poly) return
    polygons_container.removeChild(poly.graphics)
    polygons.splice(idx, 1)
}

// CUT POLY ===================================================================

function cut_poly(poly_idx) {
  const n_polys = polygons.length
  if (n_polys === 0) return
  const poly = polygons[poly_idx]
  if (!poly) return

  // u, v <- two random points along circumference iterated a bunch of times, keep the two with the smallest euclidean distance
  let smallest_dist = Number.MAX_SAFE_INTEGER
  let found = false
  let uvert = pt(0, 0)
  let vvert = pt(0, 0)
  let uidx
  let vidx
  let tmpuvert = pt(0, 0)
  let tmpvvert = pt(0, 0)
  for (let i = 0; i < 10; i++) {
    const pct_u = Math.random()
    const pct_v = pct_u+params.cut_ratio
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

  perform_cut(poly, poly_idx, uvert, vvert, uidx, vidx)
}

function perform_cut(poly, poly_idx, uvert, vvert, uidx, vidx) {
  const line = mk_line(uvert, vvert)
  lines.push(line)
  flash_lines.push({
    u: line.u,
    v: line.v,
    ticks_left: FLASH_LINE_INITIAL_TICKS_LEFT
  })

  const debug_line_graphic = new PIXI.Graphics()
  debug_line_graphic.lineStyle(1, 0xff00ff)
  debug_line_graphic.moveTo(line.u.x, line.u.y)
  debug_line_graphic.lineTo(line.v.x, line.v.y)
  debug_lines_container.addChild(debug_line_graphic)

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

  const p1 = mk_poly(p1_verts)
  const p2 = mk_poly(p2_verts)
  if (p1.circumference <= 0 || p2.circumference <= 0) return
  set_child_colour_based_on_parent_colour(poly, p1)
  set_child_colour_based_on_parent_colour(poly, p2)
  delete_poly_at(poly_idx)
  polygons_container.addChild(p1.graphics)
  polygons_container.addChild(p2.graphics)
  const p1_idx = insert_poly(p1)
  const p2_idx = insert_poly(p2)

  const command = {
    poly_idx: poly_idx,
    poly: poly,
    p1_idx: p1_idx,
    p1: p1,
    p2_idx: p2_idx,
    p2: p2,
    line: line,
    debug_line_graphic: debug_line_graphic
  }
  if (last_executed_command_idx < history.length - 1) { // not at the head, need to throw away the commands after the current pos before adding the command
    history = history.slice(0, last_executed_command_idx + 1)
  }
  history.push(command)
  last_executed_command_idx++
  canvas_container_el.dispatchEvent(new CustomEvent("history_add_command", {
    detail: {
      last_executed_command_idx: last_executed_command_idx,
      history_size: history.length
    }
  }))
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

function set_child_colour_based_on_parent_colour(parent_poly, child_poly) {
  const h = (parent_poly.h + lerp(params.hue_delta.min, params.hue_delta.max, Math.random()) + 1)%1
  const s = 1
  const l = clamp(0, 1, parent_poly.l + lerp(params.lightness_delta.min, params.lightness_delta.max, Math.random()))
  set_poly_colour(child_poly, h, s, l)
}

function set_poly_colour(poly, h, s, l) {
  poly.h = h
  poly.s = s
  poly.l = l
  poly.hex = hsl_to_hex(h, s, l)
  poly.graphics.clear()
  poly.graphics.beginFill(poly.hex).lineStyle(0).drawPolygon(poly.verts).endFill()
}

function mk_poly(verts) {
  const ret = {
    circumference: 0,
    verts: verts,
    line_length_to_idx: [],
    h: 0,
    s: 0,
    l: 0,
    hex: 0,
    graphics: new PIXI.Graphics()
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

function lines_intersection_point(p, q, r, s) {
  const denominator = ((s.y - r.y) * (q.x - p.x)) - ((s.x - r.x) * (q.y - p.y))
  if (denominator == 0) return null
  let a = p.y - r.y
  let b = p.x - r.x
  const numerator1 = ((s.x - r.x) * a) - ((s.y - r.y) * b)
  const numerator2 = ((q.x - p.x) * a) - ((q.y - p.y) * b)
  a = numerator1 / denominator
  b = numerator2 / denominator

  const x = p.x + (a * (q.x - p.x))
  const y = p.y + (a * (q.y - p.y))
  if (a < 0 || a > 1 || b < 0 || b > 1) return null
  return new PIXI.Point(x, y)
}

function random_int_in_range_inclusive(min, max) {
  min = Math.ceil(min)
  max = Math.floor(max)
  return Math.floor(Math.random() * (max - min + 1)) + min
}

function clamp(min, max, n) {
  return Math.min(Math.max(n, min), max)
}

function lerp(a, b, t) {
  return a*(1-t)+b*t
}

function ease_in_quad(t) {
  return t*t
}

function ease_out_quad(t) {
  return t*(2-t)
}

function distance_squared(v0, v1) {
  const a = v1.x - v0.x
  const b = v1.y - v0.y
  return a*a + b*b
}
