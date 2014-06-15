//list of all settings widgets
var settings_widgets = {};
var active_rollout_item=false;
//thresholds for matching and making visible/invisible
var ADD_THRESHOLD = 6;
var MAX_VISIBLE_WIDGETS = 5;

/*
 * Widget rolling down from settings panel
 */
function Settings_widget(name, keywords, description, url) {
    this.name = name;
    this.keywords = keywords;
    this.last_matched_keywords = [];
    this.description = description;
    this.url = url;
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
        var match = str_match(this.keywords[i], string) + i; 
        this.last_matched_keywords[i] = {key: this.keywords[i], val: match};
    }
    //sort from min to max
    var lmk = this.last_matched_keywords.sort(function(a, b) {return a.val-b.val});
    return lmk[0].val;
};

//draws settings window according to sellected rollout item - settings widget
function draw_settings_div() {
    $('#control_panel > input').blur();
    var name = $('.active_rollout_item').attr('id').replace('rollout_item_','');
    var settings_window = $('#settings_window');
    settings_window.css('visibility','visible');
    settings_window.css('opacity',0.9);
    $('#settings_window > .content').load(settings_widgets[name].url);
};

//emphasize the actie rollout item
function activate_rollout_item(div){
    if(active_rollout_item) {
        active_rollout_item.removeClass("active_rollout_item");
        active_rollout_item=false;
    }
    active_rollout_item = div;
    div.addClass("active_rollout_item")
}

//add rollout item to rollout menu
function add_rollout_item(widget) {
    var rollout_item = $("<div>", {class:"rollout_item", id:"rollout_item_"+widget.name});
    rollout_item.mousemove(function(){activate_rollout_item($(this))});
    rollout_item.click(function(){draw_settings_div()});
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

//init function
function init_control_panel() {
    $('#settings_window > .button').click(function(){ 
        var settings_window = $('#settings_window');
        settings_window.css('opacity', 0.0); 
        settings_window.css('visibility', 'hidden')});
    init_settings_widgets();
    function on_blur() {
        if(this.value == ''){
            this.value = 'settings..';
        }
        $("#rollout_menu").hide(100);
    };
    function on_focus() {
        $("#rollout_menu").show();
        if(this.value == 'settings..') {
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

    $("#control_panel > input").blur(on_blur);
    $("#control_panel > input").focus(on_focus);
    $("#control_panel > input").keyup(function(event){
        if(event.keyCode == 13) { //enter
            draw_settings_div();
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

//sets possible widgets and rollout items
function init_settings_widgets() {
    settings_widgets["Video"] = new Settings_widget("Video", ["video", "screen", "desktop"],
        "change video settings, share desktop", "/settings_widgets/video.html"); 
    settings_widgets["Audio"] = new Settings_widget("Audio", ["audio", "mute", "volume"],
        "change volume, mute and audio settings", "/settings_widgets/audio.html"); 
    settings_widgets["Record"] = new Settings_widget("Record", ["record", "video", "audio"],
        "record to file what you hear and see", "/settings_widgets/record.html"); 
    settings_widgets["Admin"] = new Settings_widget("Admin", ["admin", "password", "room"],
        "administrate this room, set up password", "/settings_widgets/admin.html"); 
};

init_control_panel();
