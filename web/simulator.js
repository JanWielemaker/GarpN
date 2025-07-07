		 /*******************************
		 *        PLOTLY SUPPORT        *
		 *******************************/

function initRulers(id) {
  const plot  = document.getElementById(id);
  const hrule = document.getElementById("hrule");
  const vrule = document.getElementById("vrule");
  plot.onmousemove = ev => {
    const rect = plot.getBoundingClientRect();
    hrule.style.top  = ev.clientY-rect.top + "px";
    vrule.style.left = ev.clientX-rect.left + "px";
    return true;
  };
}

let plotly_clicked_at;		// hack. How to pass info properly?

function initShapes(id) {
  const div = document.getElementById(id);

  div.addEventListener("click", (ev) => {
    const loc = getPlotCoordinates(div,ev);
    plotly_clicked_at = loc;
    htmx.trigger(div, "clicked-x", loc);
  });
}

function getPlotCoordinates(div, ev)
{ var xaxis = div._fullLayout.xaxis;
  var yaxis = div._fullLayout.yaxis;
  var l     = div._fullLayout.margin.l;
  var t     = div._fullLayout.margin.t;

  var x = xaxis.p2c(ev.x - l);
  var y = yaxis.p2c(ev.y - t);

  return { x, y };
}

function clear_output()
{ document.getElementById("errors").innerHTML = "";
  document.getElementById("status").innerHTML = "";
  document.getElementById("results").innerHTML = "";
  document.querySelectorAll("div.ml-error").forEach((e) => {
    e.remove();
  });
}

function setModel(name, opts)
{ opts = opts||{};
  document.getElementById("model").value = name;
  clear_output();

  function setOpt(sel, value) {
    if ( value ) {
      const elem = document.querySelector(sel);
      if ( elem )
	elem.value = value;
    }
  }
  function setText(sel, value) {
    if ( value ) {
      const elem = document.querySelector(sel);
      if ( elem )
	elem.textContent = value;
    }
  }

  setOpt("input[name=\"iterations\"]", opts.iterations);
  setOpt("select[name=\"method\"]",    opts.method);
  setText("#model-name",               opts.title||get_model_title(name));
}

function currentModel()
{ return document.getElementById("model").value;
}

		 /*******************************
		 *       QUANTITY SPACES        *
		 *******************************/

function q_get_value(elem) {
  const eattr = elem.querySelector("span.entity-attr");
  const eent  = elem.querySelector("span.entity");
  if ( eattr && eent ) {
    return { entity: eent.textContent,
	     attrib: eattr.textContent
	   }
  } else
  { const eplain = elem.querySelector("span.q-plain");
    return eplain.textContent;
  }
}

/** Return the currently represented points of the quantity space.
 */

function qspace_get_value(elem)
{ const q = q_get_value(elem.querySelector("span.qspace-quantity"));
  const res = { qspace_id: elem.id,
		quantity: q,
		values: []
	      };
  const il = elem.querySelectorAll("input");
  for(let i=0; i<il.length; i++)
    res.values.push(il[i].value);

  return res;
}

function get_qspaces(elem)
{ elem = elem||document.getElementById("qspace-controls");
  const qspaces = [];
  if ( elem )
  { const qcontrols = elem.querySelectorAll("div.qspace-control");
    for(let i=0; i<qcontrols.length; i++) {
      qspaces.push(qspace_get_value(qcontrols[i]));
    }
  }

  return qspaces;
}

function get_jqspaces(elem)
{ return JSON.stringify(get_qspaces(elem));
}

function get_iterations() {
  const elem = document.querySelector("input[name=\"iterations\"]");
  return elem ? elem.value : 1000;
}

function get_method() {
  const elem = document.querySelector("select[name=\"method\"]");
  return elem ? elem.value : "euler";
}

function get_model_title(model) {
  const elem = document.getElementById("model-menu");
  if ( elem ) {
    model = model || elem.value;
    const opt = elem.querySelector(`option[value="${model}"]`);
    if ( opt )
      return opt.innerText.trim();
  }
}

/**
 * Used to deal with (=) (Equal)  relations in the model.  We find the
 * input  elements, disable  the 2nd  and  add an  event handler  that
 * propagates the first to the second.
 */

function set_qspace_equalities(data) {
  const controls = document.getElementById("qspace-controls");

  function nth_child(elem) {
    let i=0;
    while((elem=elem.previousSibling)!=null)
      ++i;
    return i;
  }

  function qspace_input(qspace, pt) {
    const control = controls.querySelector("#"+qspace);
    if ( control )
    { const el = control.querySelector("[name='"+pt+"']");
      if ( el )
	return { elem: el, index: nth_child(control) };
    }
  }

  function propagate(el1, el2) {
    for(ev of ["change", "keydown", "paste", "input"]) {
      el1.addEventListener(ev, () => {
	const v1 = el1.value;
	if ( el2.value != v1 )
	  el2.value = v1;
      });
    }
    el2.disabled = true;
  }

  for(const eq of data) {
    const el1 = qspace_input(eq.qspace1, eq.qval1);
    const el2 = qspace_input(eq.qspace2, eq.qval2);

    if ( el1 && el2 ) {
      if ( el1.index < el2.index )
	propagate(el1.elem, el2.elem);
      else
	propagate(el2.elem, el1.elem);
    }
  }
}
