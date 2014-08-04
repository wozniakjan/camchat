function init_values() {
    console.log("init()");
    //1. volume 
    $('#bottom_volume .control').width($("#bottom_volume .slider").width()*get_volume('myself'));
    //2. gain
    $('#bottom_gain .control').width($("#bottom_gain .slider").width()*get_gain());
    if(get_gain() == -1) {
        $('#bottom_gain').addClass('disabled');
    }
    //3. directors cut
    $('#bottom_cut .toggler_left').addClass('toggler_on');
    //4. lock
    if(sessionStorage.room_key) {
        $('#bottom_lock .toggler_left').addClass('toggler_on');
    } else {
        $('#bottom_lock .toggler_right').addClass('toggler_on');
    }
}

function init_callbacks() {
    //1. volume & 2. gain
    $('#bottom_panel .slider').each(function(){ $(this).mousedown(function(e) {
        if( !$(this).hasClass('disabled') ){
            change_slider($(this).children('.control'), e, 'myself');
        }
    })});
    //3. directors cut
    //4. lock
    //5. fullscreen
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

//init function
function init_bottom_panel() {
    $("#bottom_panel").hover(init_values, function(){});
    init_callbacks();
}

init_bottom_panel();
