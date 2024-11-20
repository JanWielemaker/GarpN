		 /*******************************
		 *       MATHLIFE SUPPORT       *
		 *******************************/

function ml_prep(mf)
{ mf.macros = {
    ... mf.macros,
    prop:     '\\,\\text{#1}^{#2}\\,{}',
    variable: '\\,\\text{#1}'
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
    ml_prep(mf);
  }
  eql.value = function() {
    return ml_value(eql);
  }
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
