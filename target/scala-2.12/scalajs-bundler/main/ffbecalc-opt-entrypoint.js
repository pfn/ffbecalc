module.exports = {
  "require": (function(x0) {
    return {
      "snabbdom/modules/class": require("snabbdom/modules/class"),
      "snabbdom/h": require("snabbdom/h"),
      "rxjs/Rx": require("rxjs/Rx"),
      "snabbdom": require("snabbdom"),
      "snabbdom/modules/eventlisteners": require("snabbdom/modules/eventlisteners"),
      "snabbdom/modules/attributes": require("snabbdom/modules/attributes"),
      "snabbdom/modules/props": require("snabbdom/modules/props")
    }[x0]
  })
}