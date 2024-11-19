		 /*******************************
		 *       MATHLIFE SUPPORT       *
		 *******************************/

function ml_init()
{ const eql = document.getElementById("equations");
  for(eq of eql.children) {
    const mf = eq.querySelector("math-field");
    mf.macros = {
      ... mf.macros,
      prop: '\\,\\text{#1}^{#2}\\,{}',
      unit: '\\text{#1}',
    };
  }
}
