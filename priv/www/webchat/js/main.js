var room = window.location.pathname.replace(/\//g,'');
var sock = new SockJS('http://localhost:8080/sockjs/camchat');
var peer_connection = {};
var local_stream;
var my_id;
var audio_worker = new Worker("/webchat/js/audio_energy_worker.js");

audio_worker.onmessage = function(event) { 
    if(event.data.set_main){
        switch_main(event.data.set_main);
    }
};

function error_callback(error) {
    console.log(error);
}

function get_max(hashmap) {
    var max_val;
    var max_key;
    for(i in hashmap) {
        if(max_val < hashmap[i] || max_val == undefined){
            max_val = hashmap[i];
            max_key = i;
        }
    }
    return {key: max_key, val: max_val};
}

var pc_config = webrtcDetectedBrowser === 'firefox' ?
    {'iceServers':[{'url':'stun:23.21.150.121'}]} : // number IP
    {'iceServers': [{'url': 'stun:stun.l.google.com:19302'}]};

$.getScript("/webchat/js/control_panel.js");
$.getScript("/webchat/js/video.js");

function send(json_msg){
    sock.send(JSON.stringify(json_msg));
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
        setup_videos(json_msg.user_id, json_msg.peer_list, json_msg.connected);
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
    console.log('sockjs close');
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
        console.log('Remote stream added.');
        attachMediaStream(remote_video, event.stream);
        remote_video.play();
    }
    pc.onremovestream = function(event) {
        console.log('Remote stream removed.');
    }

    console.log('Adding local stream: ' + id);
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
