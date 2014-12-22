//!javascript
// prevents previously-opened tabs from reloading all at once after a restart.
execute("set load-on-focus true");

var sigId = Signal.connect("navigation", function(wv) {
        if (wv == tabs.current)
        {
            execute("set load-on-focus false");
            Signal.disconnect(sigId);
        }
});
