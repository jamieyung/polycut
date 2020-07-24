exports.init_no_ui_slider = function (st) {
  return function () {
    var el = document.getElementById(st.id)

    var range = {}
    range.min = st.range.min
    for (x of st.range.non_linear) {
      range[x.k] = [x.v, x.step]
    }
    range.max = st.range.max

    var slider = noUiSlider.create(el, {
      range: range,
      behaviour: "drag",
      start: st.start,
      connect: true,
      format: {
        to: function (v) { return v },
        from: function (v) { return Number(v) }
      }
    })

    slider.on("update", function (values) {
      el.dispatchEvent(new CustomEvent("slider_update", { detail: values }))
    })

    return el
  }
}

exports.to_event_target = function (el) {
  return el
}

exports.slider_update_listener = function (f) {
  return function (event) {
    return f(event.detail)
  }
}
