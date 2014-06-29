var room = window.location.pathname.replace(/\//g,'');
var sock = new SockJS('/sockjs/camchat');
var peer_connection = {};
var peer_last_change_stream = {};
var local_stream = {};
var my_id;
var audio_worker = new Worker("/webchat/js/audio_energy_worker.js");
var number_of_peers = 0;
var LOG_LEVEL = 9;

function log(string, priority) {
    if(priority < LOG_LEVEL) {
        console.log(string);
    }
};

audio_worker.onmessage = function(event) { 
    log("audio_worker.onmessage" + event.data, 3);
    if(event.data.set_main){
        switch_main(event.data.set_main);
    }
};

function error_callback(error) {
    if(error.name == "PermissionDeniedError") {
        show_message("Can't get audio & video", "did you allow your browser to use camera and mic?");        
    } else if(error.name == "DevicesNotFoundError") {
        show_message("Can't get audio & video", "do you have any camera or mic connected?");
    } else if(error.name == "InvalidStateError") {
        show_message("Can't get audio & video", "are you connected via https?");
    } else {
        console.log(error, 0);
    }
}

var pc_config = webrtcDetectedBrowser === 'firefox' ?
    {'iceServers': [{'url': 'stun:23.21.150.121'}]} : // number IP
    {'iceServers': [{'url': 'stun:stun.l.google.com:19302'}]};

$.getScript("/webchat/js/control_panel.js");
$.getScript("/webchat/js/video.js");

function send(json_msg){
    sock.send(JSON.stringify(json_msg));
};

function send_audio_worker(msg){
    audio_worker.postMessage(msg);
};

sock.onopen = function() {
    sock.send(JSON.stringify({'connect': room}));
};

sock.onmessage = function(e) {
    var json_msg = jQuery.parseJSON(e.data);
   
    if(json_msg.audio_energy){
        send_audio_worker(json_msg);
    } else if(json_msg.peer_connected) {
        add_peer(json_msg.peer_connected, json_msg.name);
    } else if(json_msg.connected){
        setup_videos(json_msg.user_id, json_msg.user_name, json_msg.peer_list, json_msg.connected);
    } else if(json_msg.peer_disconnected) {
        remove_peer(json_msg.peer_disconnected);
    } else if(json_msg.offer){
        parse_offer(json_msg);
    } else if(json_msg.answer){
        var pc = peer_connection[json_msg.callee];
        pc.setRemoteDescription(new RTCSessionDescription(json_msg.answer));
    } else if(json_msg.ice_candidate){
        var pc = peer_connection[json_msg.caller];
        var candidate = new RTCIceCandidate({sdpMLineIndex:json_msg.ice_candidate.label,
                                            candidate:json_msg.ice_candidate.candidate});
        pc.addIceCandidate(candidate);
    } else if(json_msg.change_name){
        change_name(json_msg.change_name, json_msg.id);
    } else if(json_msg.init_stream) {
        init_video(json_msg.init_stream);
    } else if(json_msg.select_stream) {
        change_peer_stream(json_msg.id, json_msg.select_stream);
    } else if(json_msg.error == "wrong_password") {
        ask_password();
    }
};

sock.onclose = function() {
    show_message("Disconnected");
};

function parse_offer(json_msg){
    var pc = peer_connection[json_msg.caller];
    pc.setRemoteDescription(new RTCSessionDescription(json_msg.offer), function() {
        pc.createAnswer(function(answer) {
            pc.setLocalDescription(new RTCSessionDescription(answer), function() {
                send({'answer':answer, 'caller':json_msg.caller, 'callee':json_msg.callee});
            }, error_callback);
        }, error_callback);
    }, error_callback);
};

function setup_peer_connection(id, remote_video) {
    log('setup_peer_connection(' + id + ')', 1);
    var pc = peer_connection[id] = new RTCPeerConnection(pc_config);

    pc.onicecandidate = function(event) {
        if (event.candidate) {
            send({ice_candidate: {
                    label: event.candidate.sdpMLineIndex,
                    id: event.candidate.sdpMid,
                    candidate: event.candidate.candidate},
                caller: my_id,
                callee: id});
        } 
    }
    pc.onaddstream = function(event) {
        log('pc.onaddstream', 2);
        if(peer_last_change_stream[id] == event.stream.id || 
           peer_last_change_stream[id] == undefined){
            peer_last_change_stream[id] = event.stream.id;
            attachMediaStream(remote_video, event.stream);
            remote_video.play();
        }
    }
    pc.onremovestream = function(event) {
        log('pc.onremovestream', 2);
    }

    for(var i in local_stream){
        pc.addStream(local_stream[i]);
    }
    negotiate_connection(id);
    send({'select_stream': stream_id[current_stream]});
}

function negotiate_connection(remote_id, force){
    log("negotiate_connection("+remote_id+")", 1);
    if((my_id > remote_id) || (force == true)){
        var pc = peer_connection[remote_id];
        pc.createOffer(function(offer) {
            pc.setLocalDescription(new RTCSessionDescription(offer), function() {
                send({'offer':offer, 'caller':my_id, 'callee':remote_id});
            }, error_callback);
        }, error_callback);
    }
}

//shows message window over the screen with text until it is hidden
function show_message(text, hint) {
    $("#message_window").fadeIn("slow");
    message = text;
    if(hint != undefined) message += "<br>" + hint;
    $("#message_window > .description").html(message);
}

//updates message window over the screen with text if is visible
function update_message(text, hint) {
    message = text;
    if(hint != undefined) message += "<br>" + hint;
    $("#message_window > .description").html(message);
}

//hide message window
function hide_message(text) {
    $("#message_window > .description").html(text);
    $("#message_window").fadeOut("slow");
}

//ask for password
function ask_password(){
    log("ask_password",0);
    var ask_password = $("<div>", {id: "ask_password_window"});
    var description = $("<div>", {class: "description"});
    description.html("Password needed");
    var input_div = $("<div>");
    var input = $("<input>", {id: "ask_password", class: 'password', maxlength:"20"});
    input.change(function(){this.value = this.value.replace(/\W/g, '')});
    var button = $("<div>", {class: "button"});
    button.html("ok");
    
    ask_password.append(description);
    input_div.append(input);
    ask_password.append(input_div);
    ask_password.append(button);
    
    button.click(function(){
        sessionStorage.setItem("room_password", $("#ask_password").val());
        var settings_window = $(this).parent();
        settings_window.fadeOut("fast", function(){$(this).remove()});
        send_ready();
    });
    $("body").append(ask_password);
}
