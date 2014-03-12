//!javascript 

var sig = -1;
gui.entry.connect("focus-in-event", function() {
    sig = signals.connect("keyPress", function(wv, event) {
		if (event.state == (Modifier.Control | Modifier.Shift)) {
			(function(q) {q ? q.toggle() : (function (d,j) {
				j = d.createElement("script");
				j.async = true;
				j.src = "//ime.qq.com/fcgi-bin/getjs";
				j.setAttribute("ime-cfg","lt=2");
				d = d.getElementsByTagName("head")[0];
				d.insertBefore(j,d.firstChild);
			}) (document) }) (window.QQWebIME)
		}
    });
});
gui.entry.connect("focus-out-event", function() {
    signals.disconnect(sig);
});
