		 /*******************************
		 *       MATHLIFE SUPPORT       *
		 *******************************/

function ml_prep(mf)
{
  mf.macros =
    { ... mf.macros,
      prop: {
	args: 2,
	def: '\\,\\text{#1}^\\text{#2}\\,{}',
	captureSelection: true,	// not editable
	expand: false		// keep as macro
      },
      variable: {
	args: 1,
	def: '\\,\\text{#1}',
	captureSelection: true,
	expand: false
      }
    };

  mf.addEventListener("input", (ev) => {
    const eql = ev.target.closest(".equations");
    if ( eql )
      eql.dispatchEvent(new Event('input', { bubbles: true }));
  });
}

function ml_init()
{ const eql = document.getElementById("equations");
  for(eq of eql.children) {
    const mf = eq.querySelector("math-field");
    if ( mf )
      ml_prep(mf);
  }
  eql.value = function() {
    return ml_value(eql);
  }

  activatePlusButton(eql);
}

function ml_value(eql)
{ const val = [];

  eql = eql||document.getElementById("equations");
  for(eq of eql.children) {
    const mf = eq.querySelector("math-field");
    val.push(mf.getValue(/*'math-json'*/));
  }
  return val;
}

function ml_value_string(eql)
{ return ml_value(eql).join("\v");
}

function
activatePlusButton(eql)
{ const el = eql.querySelector("div.add-equation");
  if ( el ) {
    el.addEventListener("click", (ev) => {
      const div = document.createElement("div");
      div.classList.add("equation");
      div.innerHTML = "<math-field></math-field>";
      el.parentNode.insertBefore(div, el);
      const mf = div.querySelector("math-field");
      ml_prep(mf);
    });
  }
}
