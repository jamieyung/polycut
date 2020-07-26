// HSL TO HEX =================================================================

// hsl logic based on
// https://stackoverflow.com/questions/36721830/convert-hsl-to-rgb-and-hex

// h,s,l in [0, 1]
exports.hsl_to_hex = function (h, s, l) {
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

exports.hsl_to_rgb = function (h, s, l) {
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
  return [Math.round(r*255), Math.round(g*255), Math.round(b*255)]
}

function hue_to_rgb(p, q, t) {
  if (t < 0) t += 1
  if (t > 1) t -= 1
  if (t < 1 / 6) return p + (q - p) * 6 * t
  if (t < 1 / 2) return q
  if (t < 2 / 3) return p + (q - p) * (2 / 3 - t) * 6
  return p
}

