
//!javascript 

var sig = -1;
gui.entry.connect("focus-in-event", function() {
    sig = signals.connect("keyPress", function(wv, event) {
        if (event.name == "Return") 
        {
            if (event.state == Modifier.Control)
                gui.entry.text += ".com";
            if (event.state == (Modifier.Control | Modifier.Shift))
                gui.entry.text += ".org";
        }
    });
});
gui.entry.connect("focus-out-event", function() {
    signals.disconnect(sig);
});


// Maximum visible tabs
var max = 11;


var median = max/2;
var update = function(wv)
{
    var l = tabs.length;
    var n = wv.number;
    if (max >= l)
        return;
    if (wv.number < median)
        for (var i = 0; i<l; i++)
            tabs.nth(i).tabWidget.visible = i<max;
    else if (n > l - median - 1)
        for (var i = 0; i<l; i++)
            tabs.nth(i).tabWidget.visible = i>=l-max;
    else 
        for (var i = 0; i<l; i++)
            tabs.nth(i).tabWidget.visible = i>n-median && i<=n+median;
}
signals.connect("tabFocus", update);
signals.connect("createTab", function(wv) { update(tabs.current); } );


//!javascript

var createButton = function (wv, label, position, onPress)
{
    var eventBox = new GtkWidget("GtkEventBox");
    var tabLabel = new GtkWidget("GtkLabel");

    tabLabel.useMarkup = true;
    tabLabel.label = "<span foreground='#ddd' background='#333'><b>" + label + "</b></span>";

    eventBox.connect("button-press-event", onPress);

    eventBox.add(tabLabel);

    wv.tabBox.packEnd(eventBox, false, false, 0);
    wv.tabBox.reorderChild(eventBox, position);
    tabLabel.visible = true;
    eventBox.visible = true;
}
signals.connect("createTab", function(wv) {
    createButton(wv, "&#215;", 0, function() { execute((wv.number + 1) + "close"); });
    createButton(wv, "+", 0, function() { execute("tab_new"); });
});


]:exja (function(){var e=document.querySelector("[rel='next']");if(e){location=e.href;}else{var f=document.getElementsByTagName("a");var i=f.length;while((e=f[--i])){if(e.text.search(/(\b(next|more|newer)\b|^(>|>>|»|→)$|^(>|>>|»|→)|(>|>>|»|→)$)/i)>-1){location=e.href; break;}}}})();

[:exja (function(){var e=document.querySelector("[rel='prev']");if(e){location=e.href;}else{var f=document.getElementsByTagName("a");var i=f.length;while((e=f[--i])){if(e.text.search(/(\b(prev|previous|older)\b|^(<|<<|«|←)$|^(<|<<|«|←)|(<|<<|«|←)$)/i)>-1){location=e.href;break;}}}})();

tt:exja (function(){var e=document.createElement('script');e.setAttribute('language','javascript');e.setAttribute('src','//bitly.com/bookmarklet/load.js');document.body.appendChild(e);void(0);})();


// Control e:scroll_down;;
// Control y:scroll_up;;
// gt:focus_next;;
// gT:focus_prev;;
// X:set auto-load-images true;;
// x:set auto-load-images false;;
// p:set user-stylesheet-uri file:///home/paradigm/.config/dwb/customstylesheet.css;;
// P:set user-stylesheet-uri file://;;

// q         : quit
// Control j : back
// Control k : forward
// J         : focus_prev
// K         : focus_next
// b         : hints_background
// x         : close_tab
// X         : undo

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
