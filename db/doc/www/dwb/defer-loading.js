//!javascript
if (settings.loadOnFocus === false) {
    execute('local_set load-on-focus true');
}
Signal.connect('navigation', function(webview) {
    if (webview == tabs.current) {
        execute('local_set load-on-focus false');
        this.disconnect();
    }
});
