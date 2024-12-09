		 /*******************************
		 *       MATHLIFE SUPPORT       *
		 *******************************/

const keep_items = [
  //"insert",
  "cut", "copy", "paste"
];

const my_macros = {
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

/** expand_macros() expands the string using `my_macros`.
 * The must be a simpler solution.
 */

function expand_macros(s) {
  let out = "";

  for(i=0; i<s.length; i++) {
    if ( s[i] == '\\' )
    { let name = "";
      let j;

      for(j=i+1; j<s.length && s[j].match(/[a-zA-Z0-9_]/); j++) {
	name += s[j];
      }
      const m = my_macros[name];
      if ( m )
      { let exp = m.def;
	for(k=1; k<=m.args; k++) {
	  if ( s[j] == "{" ) {
	    const e = s.indexOf("}", j);
	    if ( e > 0 ) {
	      const arg = s.slice(j+1, e);
	      exp = exp.replace("#"+k, arg);
	      j = e+1;
	    } else
	      throw("Unmatched {");
	  } else
	    throw("Missing argument for "+name);
	}
	out += exp;
	i = j;
      } else
      { out += s[i];
      }
    } else
    { out += s[i];
    }
  }

  return out;
}


function insertLabel(latex, key) {
  let str = `<span class='ML__insert-template'> `+
               `${MathLive.convertLatexToMarkup(`${expand_macros(latex)}`)}`+
            `</span>`;
  if ( key )
    str += `<span class="ML__insert-label">${key}</span>`

  return str;
}

function ml_update_menu(mf)
{ const menu = mf.menuItems.filter((i) => keep_items.indexOf(i.id) >= 0);
  menu.unshift(
    { type: "submenu",
      label: "Insert Quantity",
      submenu: [
	{ label: () => insertLabel("\\prop{x}{obj}"),
	  onMenuSelect: () => mf.insert("\\prop{x}{obj}")
	}
      ]
    },
    { type: "divider"
    });
  mf.menuItems = menu;
}

function ml_prep(mf)
{
  mf.macros = { ... mf.macros, ... my_macros };

  ml_update_menu(mf);

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
