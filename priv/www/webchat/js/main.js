var room = window.location.pathname.replace(/\//g,'');
var sock = new SockJS('/sockjs/camchat');
var peer = {};
var local_stream = {};
var my_id;
var audio_worker = new Worker("/webchat/js/audio_energy_worker.js");
var number_of_peers = 0;
var LOG_LEVEL = 9;
var front_window;
var audio_worker_active = true;

//drag & drop
var drag = undefined;
var slide_callback = undefined;
var x, y;

var ice_server_list = [{url:'stun:stun01.sipphone.com'},
{url:'stun:stun.ekiga.net'},
{url:'stun:stun.fwdnet.net'},
{url:'stun:stun.ideasip.com'},
{url:'stun:stun.iptel.org'},
{url:'stun:stun.rixtelecom.se'},
{url:'stun:stun.schlund.de'},
{url:'stun:stun.l.google.com:19302'},
{url:'stun:stun1.l.google.com:19302'},
{url:'stun:stun2.l.google.com:19302'},
{url:'stun:stun3.l.google.com:19302'},
{url:'stun:stun4.l.google.com:19302'},
{url:'stun:stunserver.org'},
{url:'stun:stun.softjoys.com'},
{url:'stun:stun.voiparound.com'},
{url:'stun:stun.voipbuster.com'},
{url:'stun:stun.voipstunt.com'},
{url:'stun:stun.voxgratia.org'},
{url:'stun:stun.xten.com'},
{url: 'stun:23.21.150.121'},
{
    url: 'turn:numb.viagenie.ca',
    credential: 'muazkh',
    username: 'webrtc@live.com'
},
{
    url: 'turn:192.158.29.39:3478?transport=udp',
    credential: 'JZEOEt2V3Qb0y27GRntt2u2PAYA=',
    username: '28224511:1379330808'
},
{
    url: 'turn:192.158.29.39:3478?transport=tcp',
    credential: 'JZEOEt2V3Qb0y27GRntt2u2PAYA=',
    username: '28224511:1379330808'
}];

var pc_config = {'iceServers': ice_server_list};

function log(string, priority) {
    if(priority < LOG_LEVEL) {
        console.log(string);
    }
}

function error_callback(error) {
    var click_settings = $('<div>', {class: 'click_link'});
    click_settings.click(function(){control_panel.draw_settings_div('Audio & Video Settings')});
    click_settings.html('see settings');
    var hint = $('<div>');
    console.log(error);
    if(error.name == "PermissionDeniedError") {
        hint.html("did you allow your browser to use camera and mic?<br>");
        hint.append(click_settings);
        show_message("Can't get audio & video", hint + click_settings);
    } else if(error.name == "DevicesNotFoundError") {
        hint.html("do you have any camera or mic connected?<br>");
        hint.append(click_settings);
        show_message("Can't get audio & video", hint);
    } else if(error.name == "InvalidStateError") {
        hint.html("are you connected via https?<br>");
        hint.append(click_settings);
        show_message("Can't get audio & video", hint);
    } else {
        hint.html(error);
        show_message("Unknown error", hint)
    }
}

function send(json_msg){
    sock.send(JSON.stringify(json_msg));
}

function send_audio_worker(msg){
    audio_worker.postMessage(msg);
}

function set_audio_worker(status){
    send_audio_worker({'work': status});
    audio_worker_active = status;
    $('.auto_cut').children().toggleClass('toggler_on');
}

function set_password(password){
}

function is_audio_worker_active(){
    return audio_worker_active;
}

$(document).ready(function() {
    audio_worker.onmessage = function(event) { 
        log("audio_worker.onmessage" + event.data, 3);
        if(event.data.set_main){
            video.switch_main(event.data.set_main);
        }
    };
    //bring popups to front on click
    $('#settings_window, #message_window').mousedown(function(){
        bring_to_front($(this));
    });

    $('.draggable').mousedown(function(e) {
        drag = $(this).parent()
        if(e.offsetX==undefined){
            x = e.pageX-target.offset().left;
            y = e.pageY-target.offset().top;
        }else{
            x = e.offsetX;
            y = e.offsetY;
        };
    });

    $('body').mouseup(function(e) {
        drag = undefined;
        slide_callback = undefined;
    });
    $('body').mousemove(function(e) {
        if (drag) {
            drag.offset({
                top: e.pageY  - y,
                left: e.pageX - x
            });
        } 
        if(slide_callback) {
            slide_callback(e);
        }
    });

    sock_callbacks();
    control_panel.init();
    bottom_panel.init();
});

function bring_to_front(window_div) {
    log("bring_to_front()", 5);
    if( !window_div.is(front_window) ){
        front_window = window_div;
        window_div.parent().append(window_div);
    }
}

function sock_callbacks(){
    sock.onopen = function() {
        sock.send(JSON.stringify({'connect': room}));
    };

    sock.onmessage = function(e) {
        var json_msg = jQuery.parseJSON(e.data);

        if(json_msg.audio_energy){
            send_audio_worker(json_msg);
        } else if(json_msg.peer_connected) {
            video.add_peer(json_msg.peer_connected, 
                    json_msg.name, 
                    json_msg.browser_token,
                    json_msg.browser);
        } else if(json_msg.connected){
            video.init_peers(json_msg.user_id, json_msg.user_name, 
                    json_msg.peer_list, json_msg.connected);
        } else if(json_msg.peer_disconnected) {
            video.remove_peer(json_msg.peer_disconnected);
        } else if(json_msg.offer){
            parse_offer(json_msg);
        } else if(json_msg.answer){
            var pc = peer[json_msg.callee].connection;
            pc.setRemoteDescription(new RTCSessionDescription(json_msg.answer));
        } else if(json_msg.ice_candidate){
            var pc = peer[json_msg.caller].connection;
            var candidate = new RTCIceCandidate({sdpMLineIndex:json_msg.ice_candidate.label,
                candidate:json_msg.ice_candidate.candidate});
            pc.addIceCandidate(candidate);
        } else if(json_msg.change_name){
            change_name(json_msg.change_name, json_msg.id);
        } else if(json_msg.init_stream) {
            video.init(json_msg.init_stream);
        } else if(json_msg.select_stream) {
            video.change_peer_stream(json_msg.id, json_msg.select_stream, json_msg.stream_name);
        } else if(json_msg.error == "wrong_key") {
            ask_key();
        } else if(json_msg.room_update == 'set_key') {
            set_key(json_msg.key);
        } else if(json_msg.let_in) {
            $('#ask_key_window').fadeOut();
            set_key(json_msg.let_in);
            video.send_ready();
        } else if(json_msg.knock) {
            somebody_knocks(json_msg.knock, json_msg.username);
        } else {
            log("sock.onmessage() -- unknown message",2);
            log(json_msg,2);
        }
    };

    sock.onclose = function() {
        show_message("Disconnected");
    };
}

function parse_offer(json_msg){
    var pc = peer[json_msg.caller].connection;
    if(webrtcDetectedBrowser != 'chrome' && pc.remoteDescription){
        log("renegotiation available only on chrome", 0);
        return;
    }
    pc.setRemoteDescription(new RTCSessionDescription(json_msg.offer), function() {
        pc.createAnswer(function(answer) {
            pc.setLocalDescription(new RTCSessionDescription(answer), function() {
                send({'answer':answer, 'caller':json_msg.caller, 'callee':json_msg.callee});
            }, error_callback);
        }, error_callback);
    }, error_callback);
}

function setup_peer_connection(id, remote_video) {
    log('setup_peer_connection(' + id + ')', 1);
    var pc = peer[id].connection = new RTCPeerConnection(pc_config);

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
        log('pc.onaddstream()', 2);
        if(webrtcDetectedBrowser != 'chrome'){
            if(peer[id].has_stream){
                log('pc.onaddstream() -> non-chrome has stream already', 3);
                return;
            }
            peer[id].has_stream = true;
        }
        if(peer[id].last_change_stream == event.stream.id || 
           peer[id].last_change_stream == undefined){
            log('pc.onaddstream() -> attachMediaStream '+peer[id].last_change_stream, 3);
            peer[id].last_change_stream = event.stream.id;
            attachMediaStream(remote_video, event.stream);
            remote_video.play();
        }
    }
    pc.onremovestream = function(event) {
        if(webrtcDetectedBrowser != 'chrome'){
            peer[id].has_stream = false;
        }
        log('pc.onremovestream', 2);
    }

    for(var i in local_stream){
        pc.addStream(local_stream[i]);
    }
    negotiate_connection(id);
}

function negotiate_connection(remote_id, force){
    log("negotiate_connection("+remote_id+")", 1);
    if((my_id > remote_id) || (force == true)){
        var pc = peer[remote_id].connection;
        pc.createOffer(function(offer) {
            pc.setLocalDescription(new RTCSessionDescription(offer), function() {
                send({'offer':offer, 'caller':my_id, 'callee':remote_id});
            }, error_callback);
        }, error_callback);
    }
}

//shows message window over the screen with text until it is hidden
function show_message(text, hint) {
    $('#message_window').fadeIn('slow');
    update_message(text, hint);
}

//updates message window over the screen with text if is visible
function update_message(text, hint) {
    if(hint){
        $("#message_window > .description").html('<b>'+text+'</b> <br>')
            .append(hint);
    } else {
        $("#message_window > .description").html(text);
    }
    bring_to_front($('#message_window'));
}

//hide message window
function hide_message(text, time) {
    $("#message_window > .description").html(text);
    $("#message_window").fadeOut(time);
}

//ask for key
function ask_key(){
    log("ask_key()",0);
    var ask_key = $("<div>", {id: "ask_key_window"});
    var drag_div = $('<div>', {class: 'draggable'})
    var description = $("<div>", {class: "description"});
    description.html("Enter key or wait 'till someone lets you in");
    var input_div = $("<div>");
    var input = $("<input>", {id: "ask_key", class: 'key', maxlength:"20"});
    input.change(function(){this.value = this.value.replace(/\W/g, '')});
    var button = $("<div>", {class: "button"});
    button.html("knock");
   
    ask_key.append(drag_div);
    ask_key.append(description);
    input_div.append(input);
    ask_key.append(input_div);
    ask_key.append(button);
    button.mousedown(function(){
        sessionStorage.setItem("room_key", $("#ask_key").val());
        var settings_window = $(this).parent();
        settings_window.fadeOut("fast", function(){$(this).remove()});
        video.send_ready();
    });
    drag_div.mousedown(function(e) {
        drag = ask_key;
        if(e.offsetX==undefined){
            x = e.pageX-target.offset().left;
            y = e.pageY-target.offset().top;
        }else{
            x = e.offsetX;
            y = e.offsetY;
        };
    });
    ask_key.mousedown(function(){
        bring_to_front($(this));
    });
    $("#screen").append(ask_key);
}

//show dialog whether to let knocking person in
function somebody_knocks(id, username) {
    log("somebody_knocks("+username+")",1);
    if($('#knock').length == 0) { //check for existance
        var div = $('<div>', {id: 'knock'+id, class: 'dialog_window'});
        var drag_div = $('<div>', {class: 'draggable'});
        var description = $('<div>', {class: 'description'});
        description.html(username + " is knocking");
        var ok = $("<div>", {class: "button"});
        ok.html("let in");
        var ignore = $("<div>", {class: "button"});
        ignore.html("ignore");

        div.append(drag_div);
        div.append(description);
        div.append(ok);
        div.append(ignore);
        
        ignore.mousedown(function(){
            var settings_window = $(this).parent();
            settings_window.fadeOut("fast", function(){$(this).remove()});
        });
        ok.mousedown(function(){
            var settings_window = $(this).parent();
            settings_window.fadeOut("fast", function(){$(this).remove()});
            send({let_in: id});
        });
        drag_div.mousedown(function(e) {
            drag = div;
            if(e.offsetX==undefined){
                x = e.pageX-target.offset().left;
                y = e.pageY-target.offset().top;
            }else{
                x = e.offsetX;
                y = e.offsetY;
            };
        });
        div.mousedown(function(){
            bring_to_front($(this));
        });
        $("#screen").append(div);
    }
}

function set_key(new_key, send_flag) {
    if(new_key != '') {
        sessionStorage.setItem("room_key", new_key);
    } else {
        sessionStorage.removeItem("room_key");
    }
    if(send_flag){
        send({room_update: 'set_key', key: 'test'});
    }
    $('.lock').children().toggleClass('toggler_on');
}

function select_text() {
    _this = $("#message_window .flag")[0];
    if(document.body.createTextRange){
        var range = document.body.createTextRange();
        range.moveToElementText(_this);
        range.select();
    } else if (window.getSelection) {
        var selection = window.getSelection();
        var range = document.createRange();
        range.selectNodeContents(_this);
        selection.removeAllRanges();
        selection.addRange(range);
    }
}

function show_initial_message() {
    var loc = $('<div>', {class: "flag", html: window.location.toString()});
    loc.click(select_text);
    var hint = $('<div>', {html: "send this link:"});
    hint.append(loc);
    update_message("Waiting for others...",hint);
}
