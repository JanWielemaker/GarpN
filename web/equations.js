		 /*******************************
		 *       MATHLIFE SUPPORT       *
		 *******************************/

const keep_items = [
  //"insert",
  "cut", "copy", "paste"
];

function insertLabel(latex, key) {
  let str = `<span class='ML__insert-template'> `+
               `${MathLive.convertLatexToMarkup(`${latex}`)}`+
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
	{ label: () => insertLabel("\\text{x}^\\text{obj}"),
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
