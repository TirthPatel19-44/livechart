'use strict';
import * as $i_$002fjavascript$002esvg from "/javascript.svg";
import * as $j_internal$002d3ebfae0cba70adf981029a0da5b1e4b5ab5d02c6 from "./internal-3ebfae0cba70adf981029a0da5b1e4b5ab5d02c6.js";
function $p_Llivechart_LiveChart$package$__setCounter$1__Lorg_scalajs_dom_Element__sr_IntRef__I__V($thiz, element$1, counter$1, count) {
  var ev$1 = count;
  counter$1.sr_IntRef__f_elem = ev$1;
  element$1.innerHTML = ("count is " + counter$1.sr_IntRef__f_elem)
}
export { $p_Llivechart_LiveChart$package$__setCounter$1__Lorg_scalajs_dom_Element__sr_IntRef__I__V as $p_Llivechart_LiveChart$package$__setCounter$1__Lorg_scalajs_dom_Element__sr_IntRef__I__V };
/** @constructor */
function $c_Llivechart_LiveChart$package$() {
  /*<skip>*/
}
export { $c_Llivechart_LiveChart$package$ as $c_Llivechart_LiveChart$package$ };
$c_Llivechart_LiveChart$package$.prototype = new $j_internal$002d3ebfae0cba70adf981029a0da5b1e4b5ab5d02c6.$h_O();
$c_Llivechart_LiveChart$package$.prototype.constructor = $c_Llivechart_LiveChart$package$;
/** @constructor */
function $h_Llivechart_LiveChart$package$() {
  /*<skip>*/
}
export { $h_Llivechart_LiveChart$package$ as $h_Llivechart_LiveChart$package$ };
$h_Llivechart_LiveChart$package$.prototype = $c_Llivechart_LiveChart$package$.prototype;
$c_Llivechart_LiveChart$package$.prototype.LiveChart__V = (function() {
  document.querySelector("#app").innerHTML = (("\r\n    <div>\r\n      <a href=\"https://vitejs.dev\" target=\"_blank\">\r\n        <img src=\"/vite.svg\" class=\"logo\" alt=\"Vite logo\" />\r\n      </a>\r\n      <a href=\"https://developer.mozilla.org/en-US/docs/Web/JavaScript\" target=\"_blank\">\r\n        <img src=\"" + $i_$002fjavascript$002esvg.default) + "\" class=\"logo vanilla\" alt=\"JavaScript logo\" />\r\n      </a>\r\n      <h1>Hello Scala.js!</h1>\r\n      <div class=\"card\">\r\n        <button id=\"counter\" type=\"button\"></button>\r\n      </div>\r\n      <p class=\"read-the-docs\">\r\n        Click on the Vite logo to learn more\r\n      </p>\r\n    </div>\r\n  ");
  $m_Llivechart_LiveChart$package$().setupCounter__Lorg_scalajs_dom_Element__V(document.getElementById("counter"))
});
$c_Llivechart_LiveChart$package$.prototype.setupCounter__Lorg_scalajs_dom_Element__V = (function(element) {
  var counter = new $j_internal$002d3ebfae0cba70adf981029a0da5b1e4b5ab5d02c6.$c_sr_IntRef(0);
  element.addEventListener("click", ((e) => {
    $p_Llivechart_LiveChart$package$__setCounter$1__Lorg_scalajs_dom_Element__sr_IntRef__I__V(this, element, counter, ((1 + counter.sr_IntRef__f_elem) | 0))
  }));
  $p_Llivechart_LiveChart$package$__setCounter$1__Lorg_scalajs_dom_Element__sr_IntRef__I__V(this, element, counter, 0)
});
var $d_Llivechart_LiveChart$package$ = new $j_internal$002d3ebfae0cba70adf981029a0da5b1e4b5ab5d02c6.$TypeData().initClass({
  Llivechart_LiveChart$package$: 0
}, false, "livechart.LiveChart$package$", {
  Llivechart_LiveChart$package$: 1,
  O: 1
});
export { $d_Llivechart_LiveChart$package$ as $d_Llivechart_LiveChart$package$ };
$c_Llivechart_LiveChart$package$.prototype.$classData = $d_Llivechart_LiveChart$package$;
var $n_Llivechart_LiveChart$package$;
function $m_Llivechart_LiveChart$package$() {
  if ((!$n_Llivechart_LiveChart$package$)) {
    $n_Llivechart_LiveChart$package$ = new $c_Llivechart_LiveChart$package$()
  };
  return $n_Llivechart_LiveChart$package$
}
export { $m_Llivechart_LiveChart$package$ as $m_Llivechart_LiveChart$package$ };
//# sourceMappingURL=livechart.-Live-Chart$package$.js.map
