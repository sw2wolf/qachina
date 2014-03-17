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

bind("Control i", qqwebime, OverrideKey.insertMode);

function dispatchEvent(evts)
{
    if (util.getMode() == Modes.InsertMode)
    {
        evts.forEach(function(e) {
            util.dispatchEvent(e);
        });
    }
}

bind("Control h", dispatchEvent.bind(null, [
            { type : 8, keyval : 65288 }
    ]), OverrideKey.insertMode);
bind("Control w", dispatchEvent.bind(null, [
            { type : 8, keyval : 65288, state : Modifier.Control }
    ]), OverrideKey.insertMode);
bind("Control u", dispatchEvent.bind(null, [
            { type : 8, keyval : 65360, state : Modifier.Shift }, 
            { type : 8, keyval : 65288 }
    ]), OverrideKey.insertMode);

Signal.connect("loadFinished", function(wv) {
    wv.mainFrame.inject("document.activeElement.blur();");
});

Signal.connect("downloadStatus", function(download) {
    if (download.status == -1)
    {
        var response = io.prompt("Downloading " + download.destinationUri + " failed, retry?");
        if (/[yY]/.test(response))
        {
            tabs.current.loadUri(download.destinationUri);
        }
    }
});

// bind("shortcut", function() {
//     tabs.current.inject("QQWebIME.toggle();");
// });

//if you want to execute it in insert mode you have to pass OverrideKey.insertMode as third parameter to bind or OverrideKey.entryFocus or OverrideKey.all

//http://paste.lisp.org/display/141664
