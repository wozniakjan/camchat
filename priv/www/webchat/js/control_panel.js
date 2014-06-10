var settings_widgets = [];
var MATCH_THRESHOLD = 60;

/*
 * Widget rolling down from settings panel
 */
function Settings_widget(name, keywords) {
    this.name = name;
    this.keywords = keywords;
    this.last_matched_keywords = [];
}

/*
 * Match input at settings panel with widget keywords
 */
Settings_widget.prototype.match = function(string) {
    // Matching heuristics of string and keyword
    function str_match(keyword, string) { //Levenshtein distance
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
                cost = (str1[i-1] == str2[j-1]) ? 0:1;
                vals = [table[i-1][j]+1, table[i][j-1]+1, table[i-1][j-1]+cost];
                table[i][j] = Math.min.apply(Math, vals);
            }
        }
        return table[len1][len2];
    }

    //match string to keywords and save results to last_matched_keywords
    for(var i in this.keywords) {
        var match = str_match(this.keywords[i], string);
        this.last_matched_keywords[i] = {key: this.keywords[i], val: match};
    }

    var lmk = this.last_matched_keywords.sort(function(a, b) {return a.val-b.val});
    return lmk[0].val*10 + lmk[1].val*5 + lmk[2].val*2;
};

function init_settings_widgets() {
    settings_widgets.push(new Settings_widget("Video", ["video", "screen", "desktop"])); 
    settings_widgets.push(new Settings_widget("Audio", ["audio", "mute", "volume"])); 
    settings_widgets.push(new Settings_widget("Record", ["record", "video", "audio"])); 
    settings_widgets.push(new Settings_widget("Room", ["room", "password", "admin"])); 
};

function init_control_panel() {
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
    function get_settings_div() {
        console.log('get_settings_div()');
        $('#control_panel > input').blur();
    };
    function filter_settings(key){
        var str = $('#control_panel > input').val();
        var filtered = [];
        for(var i in settings_widgets){
            var val = settings_widgets[i].match(str);
            if(val < MATCH_THRESHOLD){
                console.log(settings_widgets[i].name);
                this.last_matched_keywords[i] = {key: this.keywords[i], val: match};
            }
        }
    };

    $("#control_panel > input").blur(on_blur);
    $("#control_panel > input").focus(on_focus);
    $("#control_panel > input").keyup(function(event){
        if(event.keyCode == 13) { //enter
            get_settings_div();
        } else { //key
            filter_settings(event.keyCode);    
        }
    });
}

init_control_panel();
