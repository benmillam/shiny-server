function set_search(v) {
  target = document.querySelectorAll('[aria-controls="DataTables_Table_1"]'); //returns a node list
  
  var e = new KeyboardEvent("keyup", {
    bubbles : true,
    cancelable : true,
    char : "",
    key : "Unidentified",
    shiftKey : false,
    keyCode : 81
  });
  
  target.forEach(function(el) {
    el.value = v;
    el.dispatchEvent(e);
  }
  );
}
