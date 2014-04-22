window.AudioContext = window.AudioContext||window.webkitAudioContext;
var AUDIO_BUFFER_SIZE = 16384; 

function get_video_callbacks(video_element) {
    function success_callback(local_media_stream) {
        local_stream = local_media_stream;
        // Add stream to div
        attachMediaStream(video_element, local_stream);
        // For audio processing
        var audioContext = new AudioContext();
        // Create an AudioNode from the stream.
        var mediaStreamSource = audioContext.createMediaStreamSource( local_stream );
        // Script processor
        var scriptProcessor = audioContext.createScriptProcessor(AUDIO_BUFFER_SIZE, 1, 1);
        scriptProcessor.onaudioprocess = audio_filter;
        // Connect it to the destination to hear yourself (or any other node for processing!)
        mediaStreamSource.connect( scriptProcessor );
        scriptProcessor.connect( audioContext.destination );
        video_element.play();
        send({'ready': room});
    }
    return {'success' : success_callback};
}

function audio_filter(event) {
    var data = event.inputBuffer.getChannelData(0);
    var energy = data[0]*data[0];
    for(var e=1; e<AUDIO_BUFFER_SIZE; e++) {
        energy += data[e]*data[e];
    }
    send({'audio_energy': energy, 'id':my_id});
}

function init_video() {
    var constraints = {video: true, audio: true};
    var video_elem = setup_myself();
    var callbacks = get_video_callbacks(video_elem);
    navigator.getUserMedia(constraints, callbacks.success, error_callback);
}

function setup_myself() {
    var div = $("<div>", {id: "myself", class: "small_video_frame"});
    var label = $("<input>", {class: "label", text: my_id});
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
    audio_worker.postMessage({'get_main': 'audio'});
    return video[0];
}

function remove_peer(id) {
    peer_connection[id] = null;
    audio_worker.postMessage({'peer_disconnected': id});
    if( $("#main_video").attr("peer_id") == id ) {
        $("#main_video").attr("peer_id", "");
        $("#main_video > .big_video").remove();
        //audio_worker.postMessage({'get_main': 'audio'});
    }
    $("#peer"+id).hide(1000, function(){$(this).remove();});
    peer_connection[id] = null;
}

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

function setup_videos(id, peer_list, type){
    my_id = id;
    var label = $("#myself > .label").val(my_id);
    label.attr("size",label.val().length-1);
    label.focus(function(){$(this).css("opacity","0.85")});
    label.focusout(function(){$(this).css("opacity","0.6")});
    label.change(function(){ send({"change_name":$(this).val(),"id":my_id}) });
    label.keyup(function(event){
        if(event.keyCode == 13) { //enter
            $(this).blur();
        } else{
            $(this).attr("size",Math.max($(this).val().length-1,1))}
    });
    if(type === 'existing_room') {
        $.each(peer_list, function(peerId, peerUserName) {
            add_peer(peerId, peerUserName);
        });
    }
}

function change_name(name, id){
    console.log("change name " + id + " " + name);
    var label = $("#peer"+id+" > .label");
    console.log(label);
    label.html(name);
}
