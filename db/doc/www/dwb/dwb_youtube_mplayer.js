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
