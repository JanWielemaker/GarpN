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

function initShapes(sel) {
  console.log("initShapes()");
  const shapes = document.querySelectorAll(sel + " g.shape-group");
  for(let i=0; i<shapes.length; i++)
  { const shape = shapes[i];
    console.log(shape);
    shape.style.cursor = 'pointer';
    shape.addEventListener("click", (ev) => {
      const text = shape.querySelector("text");
      if ( text )
	console.log("Label: ", text.textContent);
      console.log("Clicked ", ev);
    });
  }
}


function clear_output()
{ document.getElementById("errors").innerHTML = "";
  document.getElementById("results").innerHTML = "";
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
