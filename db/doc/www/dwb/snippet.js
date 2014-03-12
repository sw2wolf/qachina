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

