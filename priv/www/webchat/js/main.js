var socket_status = document.getElementById("sockjs_status");
var room = window.location.pathname.replace(/\//g,'');
var sock = new SockJS('http://localhost:8080/sockjs/camchat');

//browser compatibility
navigator.getUserMedia = navigator.getUserMedia ||
  navigator.webkitGetUserMedia || navigator.mozGetUserMedia;

sock.onopen = function() {
    console.log('sockjs: onopen()');
    sock.send('connect:' + room);
    console.log('connect:' + room);
};
sock.onmessage = function(e) {
    console.log('sockjs message', e.data);
    if(e.data.startsWith('peer_connected')) {
        add_peer('peer');
    }
};
sock.onclose = function() {
    console.log('sockjs close');
};


String.prototype.startsWith = function(prefix) {
    return this.indexOf(prefix) === 0;
}

function init_video() {
    var constraints = {video: true, audio: true};

}

function successCallback(localMediaStream) {
    window.stream = localMediaStream;
    var video = document.querySelector('main_video');
    video.src = window.URL.createObjectURL(localMediaStream);
    video.play()
}

function errorCallback(error) {
    console.log('navigator.getUserMedia error: ', error);
}

function add_peer(id) {
    var new_peer = document.createElement('div');
    new_peer.setAttribute('class', 'small_video');
    new_peer.setAttribute('id', 'peer'+id);
    new_peer.innerHTML = 'small video';

    var video_buff = document.getElementById('video_buff');
    video_buff.appendChild(new_peer);
}

