var TIMEOUT = 1000;

var current_audio_stream_key;
var peer_importance = {};
var work = true;

/*
 * get max key and value from hashmap
 */
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

/*
 * heuristics to prevent switching of video too often
 */
function test_main_importance(max, epsilon) {
    if(max.key != undefined && max.key != current_audio_stream_key) {
        if(current_audio_stream_key == undefined)
            return true;
        if(max.value - peer_importance[current_audio_stream_key] > epsilon)
            return true;
    }
    return false;
}

/*
 * send id of new main video to the listener
 */
function send_main(epsilon) {
    if(work){
        var max = get_max(peer_importance);
        //add check if has any
        if(test_main_importance(max, epsilon)){
            current_audio_stream_key = max.key;
            postMessage({'set_main': max.key});
        }
    }
}

/*
 * infinite loop that selects main video
 */
function set_main() {
    send_main(10);
    setTimeout("set_main()",TIMEOUT);
}

/*
 * parsing incomming messages
 */
function parse_msg(msg) {
    var data = msg.data;
    if(data.audio_energy){
        peer_importance[data.id] = parseFloat(data.audio_energy);
    } else if(data.get_main){
        send_main(0);
    } else if(data.peer_disconnected) {
        delete peer_importance[data.peer_disconnected];
        if(current_audio_stream_key == data.peer_disconnected)
            current_audio_stream_key = undefined;
    } else if(data.work != undefined) {
        console.log("audio_worker "+data.work);
        work = data.work;
    } else {
    }
}

/*
 * run this at load
 */
self.addEventListener('message', parse_msg, false);
set_main();
