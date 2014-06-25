window.AudioContext = window.AudioContext||window.webkitAudioContext;
var AUDIO_BUFFER_SIZE = 16384; 
var constraints = {};
constraints["camera"] = {video: true, audio: true};
constraints["screen"] = {video: {mandatory: { chromeMediaSource: 'screen'}}, audio: false};
var current_stream;

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
    if(localStorage.user_name){
        send({'ready': room, 'user_name': localStorage.user_name});
    } else {
        send({'ready': room});
    }
}

function attach_audio_processing(current_constraints, media_stream) {
    if(current_constraints.audio){
        // For audio processing
        var audioContext = new AudioContext();
        // Create an AudioNode from the stream.
        var mediaStreamSource = audioContext.createMediaStreamSource( media_stream );
        // Script processor
        var scriptProcessor = audioContext.createScriptProcessor(AUDIO_BUFFER_SIZE, 1, 1);
        scriptProcessor.onaudioprocess = audio_filter;
        // Connect it to the destination to hear yourself (or any other node for processing!)
        mediaStreamSource.connect( scriptProcessor );
        scriptProcessor.connect( audioContext.destination );
    }
}

//get stream from user media and set to video div
function set_my_media(media_type) {
    log("set_my_media("+media_type+")", 0);
    if(local_stream[media_type] == undefined) {
        var video_elem = $("#myself video")[0];
        navigator.getUserMedia(constraints[media_type], 
                function(local_media_stream){
                    // Add stream to div
                    attach_audio_processing(constraints[media_type]);
                    // select apropriate
                    local_stream[media_type] = local_media_stream;
                    attachMediaStream(video_elem, local_stream[media_type]);
                    video_elem.play();
                    send_ready();
                }, 
                error_callback);
    }
}

//initialize video div
function init_video(media_type) {
    log("init_video()", 1);
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

//change something with my video
function change_local_stream(type) {
    log("change stream " + type, 1);
    if(type == "mute") {
        constraints["camera"].audio = false;
    } else if(type == "unmute") {
        constraints["camera"].audio = true;
    } else if(type == "camera") {
        constraints["camera"].video = true;
    }
    if(type == "screen") { 
        if(local_stream["screen"]){
            //applyConstraints
        } else {
            log("set local_screen_stream ",0);
            set_my_media("screen");
            log("set local_screen_stream "+local_stream["screen"],0);
        }
        //set video_elems visibility
    } else { //change to camera input 
        if(local_stream["camera"]) {
            //applyConstraints
        } else {
            log("set local_stream ",0);
            set_my_media("camera");
            log("set local_stream "+local_stream["camera"],0);
        }
        //set video_elems visibility
    }
    
    if(type == 'screen' || type == 'camera'){
        send({'select_stream': type});
    }
    current_stream = type;
}

//switch between cam streaming and screen sharing
function toggle_local_stream(){
    if(current_stream = "camera"){
        log("toggle_local_stream() camera -> screen", 0);
        change_local_stream("screen");
    } else {
        log("toggle_local_stream() screen -> camera", 0);
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
    setup_peer_connection("screen", id, video[0]);
    send_audio_worker({'get_main': 'audio'});
    hide_message(name + " has connected");
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

    number_of_peers -= 1;
    if(number_of_peers == 0){
        show_message("Last guy disconnected, waiting for others..");
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
    if(type === 'existing_room') {
        $.each(peer_list, function(peerId, peerUserName) {
            add_peer(peerId, peerUserName);
        });
    }
    update_message("Waiting for others...");
}


//change the name label assigned to specific id
function change_name(name, id){
    $("#peer"+id+" > .label").html(name);
}
