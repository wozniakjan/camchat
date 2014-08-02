
//init function
function init_bottom_panel() {
    $("#fullscreen").click(function(){
    if(!document.fullscreenElement && 
       !document.mozFullScreenElement && 
       !document.webkitFullscreenElement && 
       !document.msFullscreenElement ) { 
        launchFullscreen();
    } else {
        exitFullscreen();
    };});
}

init_bottom_panel();
