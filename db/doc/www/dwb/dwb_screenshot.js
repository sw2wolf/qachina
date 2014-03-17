//!javascript 
// take a screenshot of currently focused tab

function screenshot() {
    var wv = tabs.current;
    var d = new Date();
    var n = d.getTime();
    var filename = system.getEnv("HOME") + "/" + wv.title.replace(/\s/g, "_") + "_" + n + ".png"; 
    var result = wv.toPng(filename);
    if (result < 0)
        io.error("Taking screenshot failed.");
    else
       io.notify("Screenshot saved at \"" + filename + "\"."); 
    
}

bind("ps", screenshot, "screenshot");