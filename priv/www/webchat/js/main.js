var socket_status = document.getElementById("sockjs_status");
var room = window.location.pathname.replace(/\//g,'');
var sock = new SockJS('http://localhost:8080/sockjs/camchat');


sock.onopen = function() {
    console.log('sockjs: onopen()');
    sock.send('connect:' + room);
    console.log('connect:' + room);
};
sock.onmessage = function(e) {
    console.log('sockjs message', e.data);
};
sock.onclose = function() {
    console.log('sockjs close');
};



