//!javascript 
// opens YouTube videos with mplayer.

function playInMplayer() {
    system.spawn("sh -c 'mplayer \"$(youtube-dl -g " + tabs.current.uri + ")\"'");
}

function downloadVideo() {
    system.spawn("sh -c 'xterm -e \"youtube-dl " + tabs.current.uri + "\"'");
}

bind("mp", playInMplayer, "play_in_mplayer");
bind(null, downloadVideo, "download_video");

//!javascript 
// opens YouTube videos with mplayer.
// var regex = new RegExp("http(.*)://www.youtube.com/watch\\?(.*&)*v=.*");

// Signal.connect("navigation", function (wv, frame, request) {
//   if (wv.mainFrame == frame && regex.test(request.uri)) 
//     system.spawn("sh -c 'mplayer \"$(youtube-dl -g " + request.uri + ")\"'");
//   return false;
// });
