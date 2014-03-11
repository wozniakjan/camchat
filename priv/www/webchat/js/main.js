var room = window.location.pathname.replace(/\//g,'');
var sock = new SockJS('http://localhost:8080/sockjs/camchat');
var peer_connection = {};
var peer_importance = {};
var local_stream;
var my_id;

var pc_config = webrtcDetectedBrowser === 'firefox' ?
    {'iceServers':[{'url':'stun:23.21.150.121'}]} : // number IP
    {'iceServers': [{'url': 'stun:stun.l.google.com:19302'}]};

sock.onopen = function() {
    sock.send(JSON.stringify({'connect': room}));
    init_video();
};
sock.onmessage = function(e) {
    //console.log(e.data);
    var json_msg = jQuery.parseJSON(e.data);
    
    if(json_msg.peer_connected) {
        add_peer(json_msg.peer_connected, json_msg.name);
    }
    else if(json_msg.connected){
        my_id = json_msg.user_id;
        $("#myself > .label").text(my_id);
        if(json_msg.connected === 'existing_room') {
            $.each(json_msg.peer_list, function(peerId, peerUserName) {
                add_peer(peerId, peerUserName);
            });
        }
    }
    else if(json_msg.peer_disconnected) {
        remove_peer(json_msg.peer_disconnected);
    }
    else if(json_msg.offer){
        var pc = peer_connection[json_msg.caller];
        console.log("Got Offer: " + pc);
        pc.setRemoteDescription(new RTCSessionDescription(json_msg.offer), function() {
            pc.createAnswer(function(answer) {
                pc.setLocalDescription(new RTCSessionDescription(answer), function() {
                    sock.send(JSON.stringify({'answer':answer, 'caller':json_msg.caller, 'callee':json_msg.callee}));
                }, error_callback);
            }, error_callback);
        }, error_callback);
    }
    else if(json_msg.answer){
        var pc = peer_connection[json_msg.callee];
        console.log("Got Answer: " + pc);
        pc.setRemoteDescription(new RTCSessionDescription(json_msg.answer));
    }
    else if(json_msg.ice_candidate){
        var pc = peer_connection[json_msg.caller];
        console.log("Got ice candidate: " + pc);
        var candidate = new RTCIceCandidate({sdpMLineIndex:json_msg.ice_candidate.label,
                                            candidate:json_msg.ice_candidate.candidate});
        pc.addIceCandidate(candidate);
    }
};
sock.onclose = function() {
    console.log('sockjs close');
};

function get_video_callbacks(video_element) {
    function success_callback(local_media_stream) {
        local_stream = local_media_stream
        attachMediaStream(video_element, local_stream);
        video_element.play();
        sock.send(JSON.stringify({'ready': room}));
    }

    return {'success' : success_callback};
}

function init_video() {
    var constraints = {video: true, audio: false};
    var video_elem = setup_myself();
    var callbacks = get_video_callbacks(video_elem);
    navigator.getUserMedia(constraints, callbacks.success, error_callback);
}

function setup_myself() {
    var div = $("<div>", {id: "myself", class: "small_video_frame"});
    var label = $("<div>", {class: "label", text: my_id});
    var video = $("<video>", {class: "small_video", muted: "true", autoplay: "true"});
    div.append(video);
    div.append(label);
    $("#video_buff").append(div);
    return video[0];
}

function add_peer(id, name) {
    var new_peer = $("<div>", {id: "peer"+id, class: "small_video_frame"});
    var label = $("<div>", {class: "label", text: name});
    var video = $("<video>", {class: "small_video", autoplay: "true"});
    new_peer.append(video);
    new_peer.append(label);
    $("#video_buff").append(new_peer);
    setup_peer_connection(id, video[0]);
    peer_importance[id] = 0;
    set_main();
    return video[0];
}

function setup_peer_connection(id, remote_video) {
    var pc = peer_connection[id] = new RTCPeerConnection(pc_config);

    pc.onicecandidate = function(event) {
        if (event.candidate) {
            sock.send(JSON.stringify({
                ice_candidate: {
                    label: event.candidate.sdpMLineIndex,
                    id: event.candidate.sdpMid,
                    candidate: event.candidate.candidate
                },
                caller: my_id,
                callee: id}));
        } else {
            console.log('End of candidates.');
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
                sock.send(JSON.stringify({'offer':offer, 'caller':my_id, 'callee':remote_id}));
            }, error_callback);
        }, error_callback);
    }
}

function remove_peer(id) {
    if( $("#main_video").attr("peer_id") == id ) {
        $("#main_video").attr("peer_id", "");
        $("#main_video > .big_video").remove();
        set_main();
    }
    $("#peer"+id).hide(1000, function(){$(this).remove();});
    peer_connection[id] = null;
    peer_importance[id] = null;
}

function error_callback(error) {
    console.log(error);
}

function switch_main(id) {
    var main_video = $("#main_video");
    var old_peer_id = main_video.attr("peer_id");
    if(old_peer_id){
        var peer_div = $("#peer"+old_peer_id);
        peer_div.show(1000, function(){
            var small_video = $("#main_video > .big_video").attr("class","small_video");
            peer_div.append(small_video);
            small_video[0].play();
        });
    }
    $("#peer"+id).hide(1000, function(){
        var big_video = $("#peer"+id+" > .small_video").attr("class","big_video");
        main_video.append(big_video);
        big_video[0].play();
    });
    main_video.attr("peer_id", id);
}

function set_main() {
    var max = get_max(peer_importance);
    var current_key = $("#main_video").attr("peer_id");
    if(max.key != undefined && max.key != current_key)
        switch_main(max.key);
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

