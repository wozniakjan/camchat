window.AudioContext = window.AudioContext||window.webkitAudioContext;
var AUDIO_BUFFER_SIZE = 16384; 
var constraints = {};
constraints["camera"] = {video: true, audio: true};
constraints["screen"] = {video: {mandatory: { chromeMediaSource: 'screen'}}, audio: false};
var current_stream;
var stream_id = {};

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
        msg.user_name = localStorage.user_name;
    } 
    if(sessionStorage.default_media_type) {
        msg.default_stream = sessionStorage.default_media_type;
    }
    if(sessionStorage.room_password) {
        msg.password = sessionStorage.room_password;
    }
    send(msg);
}

function attach_audio_processing(media_type) {
    log("attach_audio_processing("+media_type+")", 1);
    if(constraints[media_type].audio){
        // For audio processing
        var audioContext = new AudioContext();
        // Create an AudioNode from the stream.
        var mediaStreamSource = audioContext.createMediaStreamSource( local_stream[media_type] );
        // Script processor
        var scriptProcessor = audioContext.createScriptProcessor(AUDIO_BUFFER_SIZE, 1, 1);
        scriptProcessor.onaudioprocess = audio_filter;
        // Connect it to the destination to hear yourself (or any other node for processing!)
        mediaStreamSource.connect( scriptProcessor );
        scriptProcessor.connect( audioContext.destination );
    }
}

//add additional stream
function add_my_media(media_type) {
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
                    for(i in peer_connection) {
                        peer_connection[i].addStream(local_stream[media_type]);
                        negotiate_connection(i, true);
                    }
                    current_stream = media_type;
                    send({'select_stream': stream_id[media_type]});
                }, 
                error_callback);
    } else {
        log("add_my_media() already has local_stream["+media_type+"]", 1);
    }
}

//get stream from user media and set to video div
function set_my_media(media_type) {
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
function get_local_stream(type) {
    log("get_local_stream("+type+")", 1);
    if(type != current_stream) {
        log("get_local_stream() -> local_stream = "+local_stream, 1);
        log(local_stream, 1);
        if( jQuery.isEmptyObject(local_stream) ){ 
            //no previous call to getUserMedia succeeded
            current_stream = type;
            set_my_media(type);
        } else { //has at least one local_stream
            if(local_stream[type] == undefined){
                add_my_media(type);
            } else { //has this particular local_stream
                var video_elem = $("#myself video")[0];
                attachMediaStream(video_elem, local_stream[type]);
                video_elem.play();
                //notify others through server
                send({'select_stream': stream_id[type]});
                current_stream = type;
            }
        }
    }
}

//change attributes of my video
function change_local_stream(type) {
    log("change_local_stream(" + type + ")", 1);
    if(type == "mute") {
        constraints["camera"].audio = false;
    } else if(type == "unmute") {
        constraints["camera"].audio = true;
    } else if(type == "camera") {
        constraints["camera"].video = true;
    }
    if(type == "screen") { 
        get_local_stream("screen");
    } else if(type == "camera") { 
        get_local_stream("camera");
    }
    
}

//switch between cam streaming and screen sharing
function toggle_local_stream(){
    if(current_stream == "camera"){
        log("toggle_local_stream() "+current_stream+" -> screen", 0);
        change_local_stream("screen");
    } else {
        log("toggle_local_stream() "+current_stream+" -> camera", 0);
        change_local_stream("camera");
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
function add_peer(id, name) {
    var new_peer = $("<div>", {id: "peer"+id, class: "small_video_frame"});
    var label = $("<div>", {class: "label", text: name});
    var video = $("<video>", {class: "small_video", autoplay: "true"});
    new_peer.append(video);
    new_peer.append(label);
    $("#video_buff").append(new_peer);
    setup_peer_connection(id, video[0]);
    send_audio_worker({'get_main': 'audio'});
    hide_message(name + " has connected");
    peer_name[id] = name;
    number_of_peers += 1;
    send_audio_worker({'audio_energy': [0], 'id':id});
    return video[0];
}

//when peer gets disconnected, remove his video div
function remove_peer(id) {
    peer_connection[id] = null;
    send_audio_worker({'peer_disconnected': id});
    if( $("#main_video").attr("peer_id") == id ) {
        $("#main_video").attr("peer_id", "");
        $("#main_video > .big_video").remove();
    }
    $("#peer"+id).hide(1000, function(){$(this).remove();});
    peer_connection[id] = null;
    peer_name[id] = undefined;

    number_of_peers -= 1;
    if(number_of_peers == 0){
        show_message("Last guy disconnected, waiting for others..");
    }
}

//returns stream by peer_id and stream_id
function get_stream_by_id(peer, id) {
    var streams = peer_connection[peer].getRemoteStreams();
    for(var i in streams){
        if(streams[i].id == id){
            return streams[i];
        }
    }
    return undefined;
}


//switch stream of selected per
function change_peer_stream(peer_id, stream_id){
    log("change_peer_stream(" + peer_id + ", " + stream_id+")", 1);
    var selected_remote_stream = get_stream_by_id(peer_id, stream_id);
    peer_last_change_stream[peer_id] = stream_id;
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
    if(type == 'existing_room') {
        $.each(peer_list, function(peer_id, peer_user_name) {
            add_peer(peer_id, peer_user_name);
        });
    }
    update_message("Waiting for others...");
    //hide curtain
    log("setup_videos() hide curtain", 1);
    $("#curtain").fadeOut("slow");
}


//change the name label assigned to specific id
function change_name(name, id){
    peer_name[id] = name;
    $("#peer"+id+" > .label").html(name);
}
