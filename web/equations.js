		 /*******************************
		 *       MATHLIFE SUPPORT       *
		 *******************************/

function ml_init()
{ const eql = document.getElementById("equations");
  for(eq of eql.children) {
    const mf = eq.querySelector("math-field");
    mf.macros = {
      ... mf.macros,
      prop:     '\\,\\text{#1}^{#2}\\,{}',
      variable: '\\text{#1}'
    };
  }
}

function ml_value()
{ const val = [];
  const eql = document.getElementById("equations");
  for(eq of eql.children) {
    const mf = eq.querySelector("math-field");
    val.push(mf.getValue(/*'math-json'*/));
  }
  return val;
}
