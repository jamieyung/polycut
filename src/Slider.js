exports.init_no_ui_slider = function (st) {
  return function () {
    const el = document.getElementById(st.id)

    const range = {}
    for (x of st.range) {
      range[x.k] = [x.v, x.step]
    }

    const config = {
      range: range,
      behaviour: "drag",
      start: st.start.map(st.format.to),
      connect: true,
      format: {
        to: st.format.to,
        from: st.format.from
      }
    }
    if (st.show_pips) {
      config.pips = {
        mode: "range",
        density: 3,
        format: {
          to: st.format.to,
          from: st.format.from
        }
      }
    }

    const slider = noUiSlider.create(el, config)

    slider.on("update", function (values) {
      el.dispatchEvent(new CustomEvent("slider_update", { detail: values.map(st.format.from) }))
    })

    return {
      el: el,
      config: config
    }
  }
}

exports.to_event_target = function (x) {
  return x.el
}

exports.set_values = function (x, values) {
  return function () {
    x.el.noUiSlider.set(values.map(x.config.format.to))
  }
}

exports.slider_update_listener = function (f) {
  return function (event) {
    return f(event.detail)
  }
}
