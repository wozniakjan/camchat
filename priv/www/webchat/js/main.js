var room = window.location.pathname.replace(/\//g,'');
var sock = new SockJS('/sockjs/camchat');
var peer_connection = {};
var local_stream;
var my_id;
var audio_worker = new Worker("/webchat/js/audio_energy_worker.js");
var number_of_peers = 0;
var LOG_LEVEL = 0;

function log(string, priority) {
    if(priority < LOG_LEVEL) {
        console.log(string);
    }
};

audio_worker.onmessage = function(event) { 
    log("audio_worker.onmessage" + event.data);
    if(event.data.set_main){
        switch_main(event.data.set_main);
    }
};

function error_callback(error) {
    if(error.name == "PermissionDeniedError") {
        show_message("Can't get audio & video", "did you allow your browser to use camera and mic?");        
    } else {
        console.log(error);
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
    init_video();
};

sock.onmessage = function(e) {
    var json_msg = jQuery.parseJSON(e.data);
   
    if(json_msg.audio_energy){
        audio_worker.postMessage(json_msg);
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
    log("setup_peer_connection()");
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
        log('pc.onaddstream');
        attachMediaStream(remote_video, event.stream);
        remote_video.play();
    }
    pc.onremovestream = function(event) {
        log('pc.onremovestream');
    }

    log('setup_peer_connection() -> addStream(local_stream) ' + id);
    pc.addStream(local_stream);
    negotiate_connection(id);
}

function negotiate_connection(remote_id){
    if(my_id > remote_id){
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
    $("#message_window").show();
    message = text;
    if(hint) message += "<br>" + hint;
    $("#message_window > .description").html(text + "<br>" + hint);
}

//updates message window over the screen with text if is visible
function update_message(text) {
    var message_window = $("#message_window > .description");
    if(message_window.is(':hidden')){
        message_window.show();
    }
    message_window.html(text)
}

//hide message window
function hide_message(text) {
    $("#message_window > .description").html(text);
    $("#message_window").hide(3000, function(){$(this).hide();});
}
