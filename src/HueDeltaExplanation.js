const DIM = 200
const TWO_PI = 2 * Math.PI
const resolution = 2*(window.devicePixelRatio || 1)
const strip_width = 80
const saturation = 1
const lightness = 0.5

let hsl_to_rgb
let hsl_to_hex
let range_arc
let start_indicator
let radius

exports.init = function (args) {
  return function () {
    hsl_to_rgb = args.hsl_to_rgb
    hsl_to_hex = args.hsl_to_hex

    app = new PIXI.Application({
      width: DIM,
      height: DIM,
      backgroundColor: 0xffffff,
      resolution: resolution,
      antialias: true
    })

    const canvas_len = DIM + "px"
    app.view.style["max-width"] = canvas_len
    app.view.style["max-height"] = canvas_len

    initial_draw()

    const canvas_container_el = document.getElementById("hue_delta_explanation_canvas_container")
    canvas_container_el.appendChild(app.view)
  }
}

function initial_draw() {
  const scale_fac = 2
  const dim = DIM*scale_fac
  const canvas = document.createElement("canvas")
  canvas.width = dim
  canvas.height = dim
  radius = dim*0.45
  const ctx = canvas.getContext("2d")
  const bitmap = ctx.createImageData(dim, dim)
  const c = dim/2

  for (let x = 0; x < dim; x++) {
    for (let y = 0; y < dim; y++) {
      let [r, phi] = xy_to_polar(x-c, y-c)

      if (r > radius || r < radius - strip_width) continue

      let frac = rad_to_frac(phi)

      const idx = (x + (y * dim)) * 4
      const h = frac
      const rgb = hsl_to_rgb(h, saturation, lightness)
      bitmap.data[idx] = rgb[0]
      bitmap.data[idx+1] = rgb[1]
      bitmap.data[idx+2] = rgb[2]
      bitmap.data[idx+3] = 255
    }
  }

  ctx.putImageData(bitmap, 0, 0)
  const tex = PIXI.Texture.from(canvas)
  const sprite = new PIXI.Sprite(tex)
  app.stage.addChild(sprite)

  start_indicator = new PIXI.Graphics()
  start_indicator.position.x = c
  start_indicator.position.y = c
  app.stage.addChild(start_indicator)

  range_arc = new PIXI.Graphics()
  range_arc.position.x = c
  range_arc.position.y = c
  sprite.mask = range_arc
  app.stage.addChild(range_arc)

  const border_col = 0
  const border_width = 2
  const outer_border = new PIXI.Graphics()
  outer_border.lineStyle(border_width, border_col)
  outer_border.arc(c, c, radius, 0, TWO_PI)
  app.stage.addChild(outer_border)

  const inner_border = new PIXI.Graphics()
  inner_border.lineStyle(border_width, border_col)
  inner_border.arc(c, c, radius - strip_width, 0, TWO_PI)
  app.stage.addChild(inner_border)

  app.stage.scale.x = 1/scale_fac
  app.stage.scale.y = 1/scale_fac
}

exports.redraw = function(state) {
  return function () {
    const l = state.l
    const r = state.r
    const start = state.start

    range_arc.clear()
    range_arc.lineStyle(strip_width, 0)

    const range_arc_radius = radius - strip_width * 0.5

    if (l == -0.5 && r == 0.5) {
      range_arc.drawCircle(0, 0, range_arc_radius)
    } else {
      range_arc.arc(0, 0, range_arc_radius, (start + l - 0.001) * TWO_PI, (start + r + 0.001) * TWO_PI)
    }

    start_indicator.clear()
    start_indicator.lineStyle(10, hsl_to_hex((start + 0.5)%1, saturation, lightness))
    const x = Math.cos(start*TWO_PI)
    const y = Math.sin(start*TWO_PI)
    const start_indicator_r1 = radius + 1
    const start_indicator_r2 = radius + 15
    start_indicator.moveTo(x*start_indicator_r1, y*start_indicator_r1)
    start_indicator.lineTo(x*start_indicator_r2, y*start_indicator_r2)
  }
}

function xy_to_polar(x, y) {
  let r = Math.sqrt(x*x + y*y)
  let phi = Math.atan2(y, x)
  return [r, phi]
}

// takes rad in [-pi, pi], returns deg in [0, 1]
function rad_to_frac(rad) {
  return (rad + Math.PI) / TWO_PI
}
