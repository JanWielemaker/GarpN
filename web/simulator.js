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

function clear_output()
{ document.getElementById("errors").innerHTML = "";
  document.getElementById("results").innerHTML = "";
}
