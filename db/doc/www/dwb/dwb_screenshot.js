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
bind("Control i", function() {tabs.current.inject(qqwebime) }, OverrideKey.insertMode);

// function test() {
// 	io.print("hello");
// }
// bind("Control t", test, OverrideKey.insertMode);

// function injectable() {
//     alert("hello");
// }
// function callHello() {
//     tabs.current.inject(injectable);
// }
// bind("Control r", callHello, OverrideKey.insertMode);

// function foo() { 
//     tabs.current.toFoo();
// }
// bind(null, foo.debug(script), "foodebug");
// bind(null, foo, "foo");
