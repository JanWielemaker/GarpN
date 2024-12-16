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
}

function setModel(name)
{ document.getElementById("model").value = name;
  clear_output();
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
  const res = { qspace_id: q.id,
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

		 /*******************************
		 *        ERROR HANDLING        *
		 *******************************/

document.body.addEventListener('htmx:afterRequest', function(ev) {
  const xhr = ev.detail.xhr;
  if ( xhr.status != 200 && xhr.responseText )
  { const err = JSON.parse(xhr.responseText);
    let a;
    if ( err.message &&
	 (a=/:([0-9]+):([0-9]+) *Syntax error: (.*)$/.exec(err.message)) )
    { const errors = document.getElementById("errors");
      const ta = document.getElementById("model");
      const lines = ta.value.split("\n");
      let start = 0;
      let line = parseInt(a[1]);
      const col = parseInt(a[2]);
      let charno=0;

      errors.innerText = `ERROR: ${line}:${col}: Syntax error: ${a[3]}`;
      for(let i=0; i<line-1; i++)
      { start += lines[i].length+1;
      }

      if ( col == 0 )
      { while(line >= 1 && lines[line-1].length == 0)
	{ line--;
	  start--;
	}
	start--;
      } else
      { let c=0;
	const txt = lines[line-1];
	for(; c<col; charno++)
	{ if ( txt[charno] == "\t" )
	  { c |= 7;
	  }
	  c++
	}
      }
      if ( charno >= lines[line-1].length )
	charno = lines[line-1].length;
      ta.selectionStart = start+charno;
      ta.selectionEnd = start+charno+1;
      ta.focus();
    }
  }
});
