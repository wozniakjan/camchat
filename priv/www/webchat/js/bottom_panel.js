(function(bottom_panel) {
    function init_values() {
        console.log("init()");
        $('#bottom_panel .toggler_on').removeClass('toggler_on');
        //1. volume 
        $('#bottom_volume .control').width($("#bottom_volume .slider").width()*video.get_volume('myself'));
        //2. gain
        $('#bottom_gain .control').width($("#bottom_gain .slider").width()*video.get_gain());
        if(video.get_gain() == -1) {
            $('#bottom_gain').addClass('disabled');
        }
        //3. directors cut
        if(is_audio_worker_active()){
            $('#bottom_cut .toggler_left').addClass('toggler_on');
        } else {
            $('#bottom_cut .toggler_right').addClass('toggler_on');
        }
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
                control_panel.change_slider($(this).children('.control'), e, 'myself');
            }
        })});
        //3. directors cut
        $('#bottom_cut .toggler').click(function() {
            if($(this).children('.toggler_on').hasClass('toggler_left')){
                set_audio_worker(false);
            } else {
                set_audio_worker(true);
            }
        });
        //4. lock
        $('#bottom_lock .toggler').click(function() {
            if($(this).children('toggler_on').hasClass('toggler_left')){
                set_key('test_key', true);
            } else {
                set_key('', true);
            }
        });
        //5. fullscreen
        $("#fullscreen").click(function(){
            if(!document.fullscreenElement && 
               !document.mozFullScreenElement && 
                   !document.webkitFullscreenElement && 
                       !document.msFullscreenElement ) { 
                video.launch_fullscreen();
            } else {
                video.exit_fullscreen();
            };});
    }

/******************************************************************************
 * Public API                                                                 *
 ******************************************************************************/
    //init function
    bottom_panel.init = function() {
        $("#bottom_panel").hover(init_values, function(){});
        init_callbacks();
    }
})(window.bottom_panel = window.bottom_panel || {});
