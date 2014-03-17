//!javascript 
// redirects to the simple HTML view of GMail
// useful as GMail is slow on dwb and tends to crash occasionally

var regex = new RegExp("http(.*)://mail.google.com/mail/u/([0-9])/\\?.*");

Signal.connect("navigation", function (wv, frame, request) {
  if (wv.mainFrame == frame && regex.test(request.uri))  {
    io.notify("Redirecting to thw good old HTML-only view of GMail...");
    tabs.current.loadUri("https://mail.google.com/mail/u/0/h/");
  }
});
