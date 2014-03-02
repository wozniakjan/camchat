var room = window.location.pathname.replace(/\//g,'');
var sock = new SockJS('http://localhost:8080/sockjs/camchat');

navigator.getUserMedia = navigator.getUserMedia ||
      navigator.webkitGetUserMedia || navigator.mozGetUserMedia;

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
    else if(json_msg.connected === 'existing_room') {
        $.each(json_msg.peer_list, function(peerId, peerUserName) {
            add_peer(peerId, peerUserName);
        });
    }
    else if(json_msg.peer_disconnected) {
        remove_peer(json_msg.peer_disconnected);
    }
};
sock.onclose = function() {
    console.log('sockjs close');
};

function get_video_callbacks(video_element) {
    function success_callback(localMediaStream) {
        window.stream = localMediaStream;
        video_element.src = window.URL.createObjectURL(localMediaStream);
        video_element.play();
    }

    function error_callback(error) {
        console.log('navigator.getUserMedia error: ', error);
    }

    return {'success' : success_callback, 'error' : error_callback};
}

function init_video() {
    var constraints = {video: true, audio: false};
    var video_elem = add_peer("myself", "");
    var callbacks = get_video_callbacks(video_elem);
    navigator.getUserMedia(constraints, callbacks.success, callbacks.error);
}


function add_peer(id, name) {
    var new_peer = $("<div>", {id: "peer"+id, class: "small_video_frame", text: name});
    var video = $("<video>", {class: "small_video"});
    new_peer.append(video);
    $("#video_buff").append(new_peer);
    return video[0];
}

function remove_peer(id) {
    $("#peer"+id).remove();
}

