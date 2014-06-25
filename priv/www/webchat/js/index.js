function join_room(default_type) {
    var address = "";
    if(default_type == "camera"){
        address = $("#chat_with_friends .room_name").val();
    } else if (default_type == "screen") {
        address = $("#share_screen .room_name").val();
    }
    connect_room(address, default_type);
}

function connect_room(room, default_type) {
    sessionStorage.setItem("default_media_type",default_type);
    window.location.href = room;
}

function empty_room() {
    address = "empty";
    connect_room(address, "camera");
}

function random_room() {
    address = "random";
    connect_room(address, "camera");
}
