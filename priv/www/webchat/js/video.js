window.AudioContext = window.AudioContext||window.webkitAudioContext;
var AUDIO_BUFFER_SIZE = 16384; 
var constraints = {};
constraints["camera"] = {video: true, audio: true};
constraints["screen"] = {video: {mandatory: { chromeMediaSource: 'screen'}}, audio: false};
var current_stream;
var stream_id = {};
var gainNode = null;
var master_volume = 1.0;

//filter for automatic directors cut among peers in conference
//according to audio energy
function audio_filter(event) {
    var data = event.inputBuffer.getChannelData(0);
    var energy = data[0]*data[0];
    for(var e=1; e<AUDIO_BUFFER_SIZE; e++) {
        energy += data[e]*data[e];
    }
    send({'audio_energy': energy});
}

function send_ready(){
    log("send_ready()", 1);
    msg = {ready: room};
    if(localStorage.user_name){
        //saved username, don't want to obtain generated
        msg.user_name = localStorage.user_name;
    } 
    if(sessionStorage.default_media_type) {
        //final decision up to the server
        msg.default_stream = sessionStorage.default_media_type;
    }
    if(sessionStorage.room_key) {
        //if has been set by previous page
        msg.key = sessionStorage.room_key;
    }
    if(!localStorage.browser_token) { 
        //to cancel echo for multiple same computer connections
        var guid = 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, function(c) {
            var r = Math.random()*16|0, v = c == 'x' ? r : (r&0x3|0x8);
            return v.toString(16);
        });
        localStorage.browser_token = guid;
    }
    msg.browser_token = localStorage.browser_token;
    send(msg);
}

// audio processing
// -> gain -> audio_worker -> destination
function attach_audio_processing(media_type) {
    log("attach_audio_processing("+media_type+")", 1);
    if(constraints[media_type].audio){
        // For audio processing
        var audioContext = new AudioContext();
        // Create an AudioNode from the stream.
        var mediaStreamSource = audioContext.createMediaStreamSource(local_stream[media_type]);
        // Script processor
        var scriptProcessor = audioContext.createScriptProcessor(AUDIO_BUFFER_SIZE, 1, 1);
        gainNode = audioContext.createGain();
        scriptProcessor.onaudioprocess = audio_filter;
        // Connect it to the destination to hear yourself (or any other node for processing!)
        mediaStreamSource.connect( gainNode );
        gainNode.connect( scriptProcessor );
        scriptProcessor.connect( audioContext.destination );
    }
}

//returns video element by peer id
function get_video_div(peer_id) {
    if( $("#main_video").attr("peer_id") == peer_id ) {
        return $("#main_video > video")[0];
    } else {
        return $('#peer'+peer_id+' > video')[0];
    }
}

//set peer video volume
function set_volume(peer_id, val) {
    log("set_volume("+peer_id+", "+val+")",3);
    if(peer_id == 'myself'){
        $('video').each(function() {
            this.volume = val;
        });
        master_volume = val;
    } else {
        get_video_div(peer_id).volume = val;
    }
}

//get video volume of all or any peer
function get_volume(peer_id) {
    if(peer_id == 'myself'){
        log("get_volume("+peer_id+") -> "+master_volume,3);
        return master_volume;
    } else {
        var volume = get_video_div(peer_id).volume;
        log("get_volume("+peer_id+") -> "+volume,3);
        return volume;
    }
}

//sets my microphone gain
function set_gain(val){
    log("set_gain("+val+") -> "+gainNode,3);
    if(gainNode){
        gainNode.gain.value = val;
    } else {
        log("set_gain() no gain node",3);
    }
}

//gets my microphone gain
function get_gain(){
    if(gainNode){
        log("get_gain() -> "+gainNode.gain.value,3);
        return gainNode.gain.value;
    } else {
        log("get_gain() -> null",3);
        return -1;
    }
}

//add additional stream
function add_my_media(media_type, on_success) {
    log("add_my_media("+media_type+")", 0);
    if(local_stream[media_type] == undefined) {
        log("add_my_media() new local_stream["+media_type+"]", 1);
        navigator.getUserMedia(constraints[media_type], 
                function(local_media_stream){
                    // set global 
                    local_stream[media_type] = local_media_stream;
                    stream_id[media_type] = local_stream[media_type].label;
                    // Add stream to div
                    attach_audio_processing(media_type);
                    // set visible
                    var video_elem = $("#myself video")[0];
                    attachMediaStream(video_elem, local_stream[media_type]);
                    video_elem.play();
                    // send to peers
                    for(i in peer) {
                        peer[i].connection.addStream(local_stream[media_type]);
                        negotiate_connection(i, true);
                    }
                    current_stream = media_type;
                    send({'select_stream': stream_id[media_type], 'stream_name':media_type});
                    if(on_success){ on_success();}
                }, 
                error_callback);
    } else {
        log("add_my_media() already has local_stream["+media_type+"]", 1);
    }
}

//get stream from user media and set to video div
function set_my_media(media_type, on_success) {
    log("set_my_media("+media_type+")", 0);
    if(local_stream[media_type] == undefined) {
        var video_elem = $("#myself video")[0];
        navigator.getUserMedia(constraints[media_type], 
                function(local_media_stream){
                    // set global
                    local_stream[media_type] = local_media_stream;
                    stream_id[media_type] = local_stream[media_type].label;
                    log("local_stream", 1);
                    log(local_stream[media_type], 1);
                    // Add stream to div
                    attach_audio_processing(media_type);
                    // select my visible
                    attachMediaStream(video_elem, local_stream[media_type]);
                    video_elem.play();
                    // negotiate with server
                    send_ready();
                    if(on_success) { on_success();}
                }, 
                error_callback);
    }
}

//initialize video div
function init_video(media_type) {
    log("init_video("+media_type+")", 1);
    var video_elem = setup_myself();
    if(media_type == "screen" || media_type == "camera"){
        current_stream = media_type;
    } else if(sessionStorage.default_media_type) {
        current_stream = sessionStorage.default_media_type;
    } else { 
        current_stream = "camera";
    }
    set_my_media(current_stream);
}

//get local stream and negotiate connection if needed
function get_local_stream(type, on_success) {
    log("get_local_stream("+type+")", 1);
    if(type != current_stream) {
        log("get_local_stream() -> local_stream = "+local_stream, 1);
        log(local_stream, 1);
        if( jQuery.isEmptyObject(local_stream) ){ 
            //no previous call to getUserMedia succeeded
            current_stream = type;
            set_my_media(type, on_success);
        } else { //has at least one local_stream
            if(local_stream[type] == undefined){
                add_my_media(type, on_success);
            } else { //has this particular local_stream
                var video_elem = $("#myself video")[0];
                attachMediaStream(video_elem, local_stream[type]);
                video_elem.play();
                //notify others through server
                send({'select_stream': stream_id[type], 'stream_name':type});
                current_stream = type;
                if(on_success) {on_success();};
            }
        }
    }
}

//change attributes of my video
function change_local_stream(type, on_success) {
    log("change_local_stream(" + type + ")", 1);
    if(type == "mute") {
        constraints["camera"].audio = false;
    } else if(type == "unmute") {
        constraints["camera"].audio = true;
    } else if(type == "camera") {
        constraints["camera"].video = true;
    }
    if(type == "screen") { 
        get_local_stream("screen", on_success);
    } else if(type == "camera") { 
        get_local_stream("camera", on_success);
    }
    
}

//switch between cam streaming and screen sharing
function toggle_local_stream(on_success){
    if(current_stream == "camera"){
        log("toggle_local_stream() "+current_stream+" -> screen", 0);
        change_local_stream("screen", on_success);
    } else {
        log("toggle_local_stream() "+current_stream+" -> camera", 0);
        change_local_stream("camera", on_success);
    }
}

//create my own video div
function setup_myself() {
    log("setup_myself()", 1);
    var div = $("<div>", {id: "myself", class: "small_video_frame"});
    var label = $("<input>", {class: "label", text: my_id});
    var video = $("<video>", {class: "small_video", muted: "true", autoplay: "true"});
    div.append(video);
    div.append(label);
    $("#video_buff").append(div);
    return video[0];
}

//when peer connects, create his video div
function add_peer(id, name, browser_token) {
    log("add_peer("+id+", "+name+", "+browser_token+")", 3);
    var new_peer = $("<div>", {id: "peer"+id, class: "small_video_frame"});
    var label = $("<div>", {class: "label", text: name});
    var peer_attr = {class: "small_video", autoplay: "true"};
    if (browser_token == localStorage.browser_token){
        log("add_peer() same_browser",4);
        peer_attr.muted = "muted";
    }
    var video = $("<video>", peer_attr);
    peer[id] = {'name': name, 'video': video[0], 'label': label, 'attr': peer_attr};
    new_peer.append(video);
    new_peer.append(label);
    $("#video_buff").append(new_peer);
    setup_peer_connection(id, video[0]);
    send_audio_worker({'get_main': 'audio'});
    hide_message(name + " has connected", 2000);
    number_of_peers += 1;
    send_audio_worker({'audio_energy': [0], 'id':id});
    return video[0];
}

//when peer gets disconnected, remove his video div
function remove_peer(id) {
    send_audio_worker({'peer_disconnected': id});
    if( $("#main_video").attr("peer_id") == id ) {
        $("#main_video").attr("peer_id", "");
        $("#main_video > .big_video").remove();
    }
    $("#peer"+id).hide(1000, function(){$(this).remove();});
    delete peer[id];

    number_of_peers -= 1;
    if(number_of_peers == 0){
        show_message("Last guy disconnected, waiting for others..");
    }
}

//returns stream by peer_id and stream_id
function get_stream_by_id(peer_id, id) {
    var streams = peer[peer_id].connection.getRemoteStreams();
    for(var i in streams){
        if(streams[i].id == id){
            return streams[i];
        }
    }
    return undefined;
}

function toggle_peer_stream(peer_id, on_success) {
    log("toggle_peer_stream("+peer_id+")", 1);
    var streams = peer[peer_id].connection.getRemoteStreams();
    var id = peer[peer_id].last_change_stream;
    var new_stream = undefined;
    for(var i in streams){
        //select different stream since we support only 2
        if(streams[i].id != id){
            new_stream = streams[i];
        }
    }
    if(new_stream){
        if(peer[peer_id].stream_name == 'camera'){
            change_peer_stream(peer_id, new_stream.id, 'screen');
        } else {
            change_peer_stream(peer_id, new_stream.id, 'camera');
        }
        on_success(); 
    } else {
        log("toggle_peer_stream() failed",1);
    }
}

//switch stream of selected per
function change_peer_stream(peer_id, stream_id, stream_type){
    if(webrtcDetectedBrowser === 'chrome'){
        log("change_peer_stream(" + peer_id + ", " + stream_id+")", 1);
        var selected_remote_stream = get_stream_by_id(peer_id, stream_id);

        peer[peer_id].last_change_stream = stream_id;
        if(selected_remote_stream != undefined) {
            var current_main = $("#main_video").attr("peer_id");
            var video_elem = undefined;
            if(current_main == peer_id){
                video_elem = $("#main_video > .big_video")[0];
            } else {
                video_elem = $("#peer"+peer_id+" > .small_video")[0];
            }
            attachMediaStream(video_elem, selected_remote_stream);
            video_elem.play();
            peer[peer_id].stream_name = stream_type;
        } 
    }
}

//switch main video div and small video div with specific id
function switch_main(id) {
    var main_video = $("#main_video");
    var old_peer_id = main_video.attr("peer_id");
    if(old_peer_id){
        var peer_div = $("#peer"+old_peer_id);
        peer_div.show(0, function(){
            var small_video = $("#main_video > .big_video").attr("class","small_video");
            peer_div.append(small_video);
            small_video[0].play();
        });
    }
    $("#peer"+id).hide(0, function(){
        var big_video = $("#peer"+id+" > .small_video").attr("class","big_video");
        main_video.append(big_video);
        big_video[0].play();
    });
    main_video.attr("peer_id", id);
}

//checks previously set user_name and saves/loads according
function get_username(user_name){
    if( !localStorage.user_name ){
        localStorage.user_name = user_name;
    }
    return localStorage.user_name;
}

function set_username(user_name) {
    localStorage.user_name = user_name;
}

//called during initialization, create divs with video
//and arrange them on the page
function setup_videos(id, user_name, peer_list, type){
    log("setup_videos()", 0);
    my_id = id;
    var my_name = get_username(user_name);
    var label = $("#myself > .label").val(my_name);
    //set label width
    label.attr("size",label.val().length);
    //send changed name to server
    label.change(function(){ set_username($(this).val()); send({"change_name":$(this).val()}) });
    //dynamic width of label for user name
    label.keyup(function(event){
        if(event.keyCode == 13) { //enter
            $(this).blur();
        } else{
            $(this).attr("size",Math.max($(this).val().length,1))}
    });
    show_initial_message();
    if(type == 'existing_room') {
        $.each(peer_list, function(peer_id, attr) {
            add_peer(peer_id, attr.user_name, attr.browser_token);
        });
    }
    //hide curtain
    log("setup_videos() hide curtain", 1);
    $("#curtain").fadeOut("slow");
}


//change the name label assigned to specific id
function change_name(name, id){
    peer[id].name = name;
    $("#peer"+id+" > .label").html(name);
}
