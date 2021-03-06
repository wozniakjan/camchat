(function(control_panel) {
    //list of all settings widgets
    var settings_widgets = {};
    var active_rollout_item=false;
    //thresholds for matching and making visible/invisible
    var ADD_THRESHOLD = 6;
    var MAX_VISIBLE_WIDGETS = 5;
    //to make rollout menu clickable
    var rollout_menu_active = false;
    //slider for changing volumes
    var slider_user_id;


    /*
     * Widget rolling down from settings panel
     */
    function Settings_widget(name, keywords, description, url, init_callback) {
        this.name = name;
        this.keywords = keywords;
        this.last_matched_keywords = [];
        this.description = description;
        this.url = url;
        this.init_callback = init_callback;
    }

    /*
     * Match input at settings panel with widget keywords
     */
    Settings_widget.prototype.match = function(string) {
        // Matching heuristics of string and keyword
        var MISS_COST = 4; //increased miss cost to prefer match over short keywords
        function str_match(keyword, string) { //modified Levenshtein distance
            if(string=="") return 10;
            if(keyword.length >= string.length){
                str1 = keyword;
                str2 = string;
            } else {
                str1 = string;
                str2 = keyword;
            }
            len1 = str1.length;
            len2 = str2.length;

            var cost;
            var table = [];
            table[0] = Array.apply(0, {length: len2+1}).map(Number.call, Number);
            for(var i = 1; i <= len1; i++){
                table[i] = [];
                table[i][0] = i;
                for(var j = 1; j <= len2; j++){
                    cost = (str1[i-1] == str2[j-1]) ? 0:MISS_COST;
                    vals = [table[i-1][j]+1, table[i][j-1]+1, table[i-1][j-1]+cost];
                    table[i][j] = Math.min.apply(Math, vals);
                }
            }
            return table[len1][len2];
        }

        //match string to keywords and save results to last_matched_keywords
        for(var i=0; i<this.keywords.length; i++) {
            var match = str_match(this.keywords[i], string); 
            this.last_matched_keywords[i] = {key: this.keywords[i], val: match};
        }
        //sort from min to max
        var lmk = this.last_matched_keywords.sort(function(a, b) {return a.val-b.val});
        return lmk[0].val;
    };

    //draws settings window according to sellected rollout item - settings widget
    function draw_active_settings_div() {
        $('#control_panel > input').blur();
        var name = $('.active_rollout_item').attr('id').replace('rollout_item_','');
        control_panel.draw_settings_div(name);
    };

    //emphasize the actie rollout item
    function activate_rollout_item(div){
        if(active_rollout_item) {
            active_rollout_item.removeClass("active_rollout_item");
            active_rollout_item = false;
        }
        active_rollout_item = div;
        div.addClass("active_rollout_item")
    }

    //add rollout item to rollout menu
    function add_rollout_item(widget) {
        var rollout_item = $("<div>", {class:"rollout_item", id:"rollout_item_"+widget.name});
        rollout_item.mousemove(function(){activate_rollout_item($(this))});
        rollout_item.click(function(){draw_active_settings_div()});
        var description = $("<div>", {class: "description"});
        description.html(widget.description);
        rollout_item.html(widget.name);
        rollout_item.append(description);
        $('#rollout_menu').append(rollout_item);
        return rollout_item;
    }

    //draw rollout items in rollout menu
    function redraw_settings_widgets(to_add){
        $('.rollout_item').remove();
        if(to_add.length>0) {
            var all_divs = [];
            var first_div = add_rollout_item(to_add[0].widget);
            activate_rollout_item(first_div);
            for(var i=1; i<to_add.length; i++){
                add_rollout_item(to_add[i].widget);
            }
        } else {
            active_rollout_item = false;
        }
    }


    //sets possible widgets and rollout items
    function init_settings_widgets() {
        settings_widgets["Audio & Video Settings"] = new Settings_widget("Audio & Video Settings", 
            ["settings", "video", "audio", "screen", "desktop", "record", "volume", "mute"],
            "share desktop, record, adjust volume", 
            "/settings_widgets/media.html", 
            media_open); 
        settings_widgets["Room Settings"] = new Settings_widget("Room Settings", 
            ["settings", "admin", "key", "room", "kick"],
            "administrate this room, set up key", 
            "/settings_widgets/admin.html", 
            room_open); 
    };

    //set slider width
    function set_slider(slider, user_id) {
        log("set_slider("+slider+", "+user_id+")",3);
        var val;
        switch(slider){
            case 'volume':
                val = video.get_volume(user_id);
            break;
            case 'gain':
                if(user_id=='myself'){
                val = video.get_gain();
            } else { //unable to set peer micro
                val = 1.0;
            }
            break;
            default:
                log("setting wrong slider",3);
        }
        $('.'+slider+' > .control').each(function() {$(this).width($(this).parent().width()*val)});
    }

    //callbacks
    function media_open() {
        log('media_open()', 3);
        function process_change(user_id, div){
            log('process_change('+user_id+', '+div.context.id+')', 3);
            if(user_id == 'myself'){
                if(div.context.id == 'stream_switch') {
                    var on_success = function(){
                        log("media_open() -> on_success() callback");
                        div.children().toggleClass('toggler_on')
                    };
                    video.toggle_local_stream(on_success);
                } else if(div.context.id == 'auto_cut') {
                    if(div.children('.toggler_on').hasClass('toggler_left')){
                        set_audio_worker(false);
                    } else {
                        set_audio_worker(true);
                    }
                } else if(div.context.id == 'record_switch'){
                    show_message("Not yet implemented");
                    div.children().toggleClass('toggler_on');
                }
            } else {
                if(div.context.id == 'stream_switch') {
                    var on_success = function(){
                        log("media_open() -> on_success() callback");
                        div.children().toggleClass('toggler_on')
                    };
                    toggle_peer_stream(user_id, on_success);
                }
            }
        }
        function draw_my_settings(){
            //1. stream selection 
            if(video.current_stream == "camera"){
                $('#stream_switch > .toggler_left').addClass('toggler_on');
            } else {
                $('#stream_switch > .toggler_right').addClass('toggler_on');
            }
            if(webrtcDetectedBrowser != 'chrome'){
                $('#stream_switch').addClass('disabled');
            }
            //2. recording video & audio
            $('#record_switch > .toggler_right').addClass('toggler_on');
            $('#record_switch').addClass('disabled');
            //3. volume
            set_slider('volume', 'myself');
            set_slider('gain', 'myself');
            if(video.get_gain() == -1) {
                $('.gain').addClass('disabled');
            }
            //4. directors cut
            $('#auto_cut > .toggler_on').removeClass('toggler_on');
            if(is_audio_worker_active()){
                $('#auto_cut > .toggler_left').addClass('toggler_on');
            } else {
                $('#auto_cut .toggler_right').addClass('toggler_on');
            }
        }
        function draw_user_settings(user_id){
            //1. stream selection
            if(peer[user_id].connection.getRemoteStreams().length < 2){
                $('#stream_switch').addClass('disabled');
            }
            if(peer[user_id].stream_name == "camera"){
                $('#stream_switch > .toggler_left').addClass('toggler_on');
            } else {
                $('#stream_switch > .toggler_right').addClass('toggler_on');
            }
            //2. recording video & audio
            $('#record_switch > .toggler_right').addClass('toggler_on');
            $('#record_switch').addClass('disabled');
            //3. volume
            set_slider('volume', user_id);
            set_slider('gain', user_id);
            $('.gain').addClass('disabled');
            //4. directors cut
            $('#auto_cut > .toggler_left').addClass('toggler_on');
            $('#auto_cut').addClass('disabled');
        }
        function draw_settings(user_id){
            log('draw_settings('+user_id+')', 3);
            $('#settings_window .toggler').unbind('mousedown').mousedown(function(){
                if( !$(this).hasClass('disabled') ){
                    process_change(user_id, $(this));
                }
            });
            $('#settings_window .toggler_on').removeClass('toggler_on');
            $('#settings_window .disabled').removeClass('disabled');
            $('#settings_window .slider').unbind('mousedown').mousedown(function(e) {
                if( !$(this).hasClass('disabled') ){
                    var slider_element = $(this).children('.control');
                    slide_callback = function(e) { 
                        control_panel.change_slider(slider_element, e, user_id);
                    };
                    control_panel.change_slider(slider_element, e, user_id);
                }
            });
            if(user_id == "myself"){
                draw_my_settings();
            } else {
                draw_user_settings(user_id);
            }
        }
        $('#select_user').children().remove().end()
        .append('<option value="myself" selected="selected">my settings</option>');
        for(var id in peer){
            $('#select_user').append('<option value="'+id+'">'+peer[id].name+'</option>');
        };
        var select_user = $('#select_user').chosen({disable_search_threshold: 10, width: '200px'});
        select_user.change(function(){ draw_settings($(this).val()); });
        draw_settings('myself');
    };

    function room_open() {
        function process_change(div){
            log('process_change('+div.context.id+')', 3);
            switch(div.context.id) {
                case 'lock_switch' :
                    if($('#lock_switch > .toggler_left').hasClass('toggler_on')){
                        set_key('', true);
                    } else {
                        set_key('test_key', true);
                    }
                break;
                default :
                    log('room_open() -> process_change() unknown div', 3);
            }
        }
        $('#settings_window .toggler').unbind('mousedown').mousedown(function(){
            if( !$(this).hasClass('disabled') ){
                process_change($(this));
            }
        });
        $('#settings_window .toggler_on').removeClass('toggler_on');
        $('#settings_window .disabled').removeClass('disabled');
        if(sessionStorage.room_key) {
            $('#lock_switch > .toggler_left').addClass('toggler_on');
        } else {
            $('#lock_switch > .toggler_right').addClass('toggler_on');
        }
    };
    
/******************************************************************************
 * Public API                                                                 *
 ******************************************************************************/
    //set slider and change its effective value
    control_panel.change_slider = function(slider, e, user_id) {
        log("change_slider",3);
        if(user_id != undefined){
            slider_user_id = user_id;
        }
        var left = slider.parent().offset().left;
        var right = left+slider.parent().width();
        var x = Math.min(right, Math.max(left, e.pageX));
        var val = (x-left) / (right-left);
        if(slider.parent().hasClass('volume')){
            video.set_volume(slider_user_id, val);
            set_slider('volume', user_id);
        } else if(slider.parent().hasClass('gain')){
            video.set_gain(val);
            set_slider('gain', user_id);
        } else {
            log("setting wrong slider",3);
        }
    }

    //must be public due to setTimeout visibility
    control_panel.try_blur_rollout_menu = function(i) {
        if(rollout_menu_active && i > 0){
            j = i-1;
            setTimeout('control_panel.try_blur_rollout_menu(j)', 5);
        } else {
            $("#rollout_menu").hide(100);
        }
    }
    
    //draws settings window according to argument
    control_panel.draw_settings_div = function (name) {
        var settings_window = $('#settings_window');
        settings_window.css('visibility','visible');
        settings_window.fadeIn("fast");
        $('#settings_window > .content').load(settings_widgets[name].url,
                function() {
                    settings_widgets[name].init_callback(); 
                    bring_to_front(settings_window);
                });
    }

    //init function
    control_panel.init = function() {
        $('.button').mousedown(function(){ 
            var settings_window = $(this).parent();
            settings_window.fadeOut("fast")});
            init_settings_widgets();
            function on_blur() {
                this.value = 'search..';
                control_panel.try_blur_rollout_menu(20);
            };
            function on_focus() {
                $("#rollout_menu").show();
                if(this.value == 'search..') {
                    this.value = '';
                } else {
                    $('#control_panel > input').select();
                }
            };
            function filter_settings(key){
                var str = $('#control_panel > input').val();
                var add = [];
                for(var i in settings_widgets){
                    var val = settings_widgets[i].match(str);
                    if(val < ADD_THRESHOLD){
                        add.push({'widget':settings_widgets[i], 'val':val});
                    }
                }
                //sort from min to max and take up to MAX_VISIBLE_WIDGETS and only good matching
                var to_add = add.sort(function(a, b) {return a.val-b.val})
                .slice(0,MAX_VISIBLE_WIDGETS);
                redraw_settings_widgets(to_add);
            };

            $("#rollout_menu").hover(
                function(){rollout_menu_active=true}, 
                function(){rollout_menu_active=false});
                $("#control_panel > input").blur(on_blur);
                $("#control_panel > input").focus(on_focus);
                $("#control_panel > input").keyup(function(event){
                    if(event.keyCode == 13) { //enter
                        draw_active_settings_div();
                    } else if(event.keyCode == 40) { //arrow down
                        var ri = active_rollout_item;
                        if(ri && ri.next().hasClass('rollout_item'))
                            activate_rollout_item(active_rollout_item.next());
                    } else if(event.keyCode == 38) { //arrow up
                        var ri = active_rollout_item;
                        if(ri && ri.prev().hasClass('rollout_item'))
                            activate_rollout_item(active_rollout_item.prev());
                    } else { //key
                        filter_settings(event.keyCode);    
                    }
                });
    }
})(window.control_panel = window.control_panel || {});
