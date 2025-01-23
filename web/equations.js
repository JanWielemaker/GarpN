		 /*******************************
		 *       MATHLIFE SUPPORT       *
		 *******************************/

const state = {};

const keep_items = [
  //"insert",
  "cut", "copy", "paste"
];

const del_shortcuts = [
  "and"
];

// (*).  Actually, we want `expand: false` to keep the macro in place.
// Unfortunately, editing (notably typing '*' to insert `\cdot`) causes
// this to loose the macro arguments, so we are left with `\prop`.  As
// a consequence, we must deal with the macro expansion in Prolog and
// keep this in sync.  Eventually, if there is no better way, we shall
// generate this macro from Prolog or generate the Prolog parsing from
// this macro.

const my_macros = {
  prop: {
    args: 2,
    def: '\\,\\text{#1}^\\text{#2}\\,{}',
    captureSelection: true,	// not editable
    expand: true		// Expand (*)
  },
  variable: {
    args: 1,
    def: '\\,\\text{#1}',
    captureSelection: true,
    expand: true
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

function ml_update_menu(mf, quantities)
{ const menu = mf.menuItems.filter((i) => keep_items.indexOf(i.id) >= 0);
  quantities = quantities||state.quantities;

  function insertQuantity(ltx) {
    return { label: () => insertLabel(ltx),
	     onMenuSelect: () => mf.insert(ltx)
	   };
  }
  function insertDerivative(ltx) {
    const dltx = ltx
	  .replace(/\\prop{/, "\\prop{Δ")
	  .replace(/\\variable{/, "\\variable{Δ");
    return insertQuantity(dltx);
  }

  menu.unshift(
    { type: "submenu",
      label: "Insert Quantity",
      submenu: quantities.map((q) => insertQuantity(q))
    },
    { type: "submenu",
      label: "Insert Derivative",
      submenu: quantities.map((q) => insertDerivative(q))
    },
    { type: "divider"
    });
  mf.menuItems = menu;
}

function eql_changed(from)
{ const eql = from.closest(".equations");
  if ( eql )
    eql.dispatchEvent(new Event('input', { bubbles: true }));
}

function ml_prep(mf, quantities)
{
  mf.macros = { ... mf.macros, ... my_macros };
  const shortcuts = { ... mf.inlineShortcuts };
  for(sc of del_shortcuts)
    delete shortcuts[sc];
  mf.inlineShortcuts = shortcuts;

  ml_update_menu(mf, quantities);

  mf.addEventListener("input", (ev) => {
    eql_changed(ev.target);
  });
}

function eq_prep(eq, quantities)
{ const mf  = eq.querySelector("math-field");
  const del = eq.querySelector("span.delete-equation");
  if ( mf )
    ml_prep(mf, quantities);

  if ( del )
  { del.addEventListener("click", (ev) =>
    { const eql = eq.parentNode;
      eq.remove();
      eql_changed(eql);
    });
  }
}

function ml_init(quantities)
{ const eql = document.getElementById("equations");
  state.quantities = quantities;

  for(eq of eql.children) {
    eq_prep(eq, quantities);
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
    if ( mf )
      val.push(mf.getValue('latex-expanded'));
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
      div.innerHTML = "<math-field></math-field>"+
	'<span class="delete-equation">✖</span>';
      el.parentNode.insertBefore(div, el);
      eq_prep(div);
    });
  }
}

/** Handle errors in the model
 *
 * @param errors is a list of errors.  Each error is an object
 * holding the expression line (1 based), the source (as LaTeX) and
 * a message.
 */

function ml_errors(errors, eql)
{ eql = eql||document.getElementById("equations");
  let sub=1

  for(e of errors)
  { const eq = eql.children[e.line-sub];
    const div = document.createElement("div");

    div.classList.add("ml-error");
    if ( e.html )
      div.innerHTML = e.html;
    else
      div.textContent = e.message;
    eq.after(div);
    sub--;
  }
}
