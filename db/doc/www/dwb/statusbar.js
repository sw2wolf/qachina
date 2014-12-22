#!javascript

gui.uriLabel.xalign = 0;

signals.connect("statusBarChange", function(wv, data) {
    // uri
    gui.uriLabel.useMarkup = true;
    var uri = util.markupEscape(wv.uri);
    if (data.isBookmarked)
        uri = "<span foreground='#916949'>" + uri + "</span>";

    // history
    if (data.canGoBack || data.canGoForward) {
        uri += " ";
        if (data.canGoBack)
            uri += "←";
        if (data.canGoForward)
            uri += "→";
    }

    // progress
    var progress = wv.progress;
    if (progress > 0 && progress < 1)
        uri += " <span foreground='#666362'>(" + Math.round(progress * 100) + "%)</span>";

    // ssl check
    var statusText = "";
    if (data.ssl == "trusted")
        statusText = "<span foreground='#6B853D'>SSL</span> ";
    else if (data.ssl == "untrusted")
        statusText = "<span foreground='#6B853D'>SSL</span> ";

    // scroll
    var adjustment = wv.parent.vadjustment;

    var lower = adjustment.lower;
    var upper = adjustment.upper - adjustment.pageSize + lower;
    var value = adjustment.value;

    if (upper == lower)
        statusText += "All";
    else if (value == lower)
        statusText += "Top";
    else if (value == upper)
        statusText += "Bot";
    else
    {
        var perc = Math.round(value * 100 / upper);
        if (perc < 10)
            statusText += " ";
        statusText += perc + "%";
    }

    uri = "<span weight='normal'>" + uri + "</span>";
    statusText = "<span weight='normal'>" + statusText + "</span>";

    gui.uriLabel.label = uri;
    gui.statusLabel.label = statusText;
    return true;
});

gui.entry.connect("focus-in-event", function() {
    gui.uriLabel.visible = false;
    gui.statusLabel.visible = false;
});

gui.entry.connect("focus-out-event", function() {
    gui.uriLabel.visible = true;
    gui.statusLabel.visible = true;
});

gui.messageLabel.notify("label", function() {
    if (gui.messageLabel.label == "") {
        gui.uriLabel.visible = true;
        gui.statusLabel.visible = true;
    }
    else {
        gui.uriLabel.visible = false;
        gui.statusLabel.visible = false;
    }
});
