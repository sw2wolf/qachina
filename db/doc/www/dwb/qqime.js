//!javascript 

function qqwebime() {
    var q = window.QQWebIME;
    q ? q.toggle() : (function (d,j) {
        j = d.createElement("script");
        j.async = true;
        j.src = "//ime.qq.com/fcgi-bin/getjs";
        j.setAttribute("ime-cfg","lt=2");
        d = d.getElementsByTagName("head")[0];
        d.insertBefore(j,d.firstChild);
    }(document));
}
var sig = -1;
gui.entry.connect("focus-in-event", function() {
    sig = Signal.connect("keyPress", function(wv, event) {
        if (event.state == (Modifier.Control | Modifier.Shift)) {
            tabs.current.inject(qqwebime);
        }
    });
});
gui.entry.connect("focus-out-event", function() {
    Signal.disconnect(sig);
});


// bind("shortcut", function() {
//     tabs.current.inject("QQWebIME.toggle();");
// });

//if you want to execute it in insert mode you have to pass OverrideKey.insertMode as third parameter to bind or OverrideKey.entryFocus or OverrideKey.all
