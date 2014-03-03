var room = window.location.pathname.replace(/\//g,'');
var sock = new SockJS('http://localhost:8080/sockjs/camchat');
var peer_connection = {};
var remote_stream = {};
var local_stream;
var my_id;

sock.onopen = function() {
    sock.send(JSON.stringify({'connect': room}));
    init_video();
};
sock.onmessage = function(e) {
    var json_msg = jQuery.parseJSON(e.data);
    console.log(json_msg);
    
    if(json_msg.peer_connected) {
        add_peer(json_msg.peer_connected, json_msg.name);
    }
    else if(json_msg.connected){
        my_id = json_msg.user_id;
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
        pc.setRemoteDescription(new RTCSessionDescription(json_msg.offer), function() {
            pc.createAnswer(function(answer) {
                pc.setLocalDescription(new RTCSessionDescription(answer), function() {
                    sock.send(JSON.stringify({'answer':answer, 'caller':json_msg.caller, 'callee':json_msg.callee}));
                }, error_callback);
            }, error_callback);
        }, error_callback);
    }
};
sock.onclose = function() {
    console.log('sockjs close');
};

function get_video_callbacks(video_element) {
    function success_callback(local_media_stream) {
        local_stream = local_media_stream
        video_element.src = window.URL.createObjectURL(local_media_stream);
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
    var div = $("<div>", {id: "myself", class: "small_video_frame", text: "Myself"});
    var video = $("<video>", {class: "small_video"});
    div.append(video);
    $("#video_buff").append(div);
    return video[0];
}

function add_peer(id, name) {
    var new_peer = $("<div>", {id: "peer"+id, class: "small_video_frame", text: name});
    var video = $("<video>", {class: "small_video"});
    new_peer.append(video);
    $("#video_buff").append(new_peer);
    setup_peer_connection(id, video);
    return video[0];
}

function setup_peer_connection(id, remote_video) {
    pc = peer_connection[id] = new RTCPeerConnection(null);

    pc.onicecandidate = function(event) {
        console.log('handleIceCandidate event: ', event);
        if (event.candidate) {
            sock.send({
                'message': 'ice_candidate', 
                type: 'candidate',
                label: event.candidate.sdpMLineIndex,
                id: event.candidate.sdpMid,
                candidate: event.candidate.candidate});
        } else {
            console.log('End of candidates.');
        }
    }
    pc.onaddstream = function(event) {
        console.log('Remote stream added.');
        remote_video.src = window.URL.createObjectURL(event.stream);
        remote_stream[id] = event.stream;
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
        pc.createOffer(function(offer) {
            pc.setLocalDescription(new RTCSessionDescription(offer), function() {
                sock.send(JSON.stringify({'offer':offer, 'caller':my_id, 'callee':remote_id}));
            }, error_callback);
        }, error_callback);
    }
}

function remove_peer(id) {
    $("#peer"+id).remove();
    peer_connection[id] = null;
}

function error_callback(error) {
    console.log(error);
}
