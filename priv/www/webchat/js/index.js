function join_room(default_type) {
    var address = "";
    if(default_type == "camera"){
        address = $("#chat_with_friends .room_name").val();
        save_password($("#chat_with_friends .password"));
    } else if (default_type == "screen") {
        address = $("#share_screen .room_name").val();
        save_password($("#share_screen .password"));
    }
    connect_room(address, default_type);
}

function save_password(div) {
    var password = div.val();
    if(password != "set password..." && password != ""){
        sessionStorage.setItem("room_password", password);
    } else {
        sessionStorage.removeItem("room_password");
    }
}


function connect_room(room, default_type) {
    sessionStorage.setItem("default_media_type",default_type);
    window.location.href = room;
}

function empty_room(default_type) {
    $("<div>").load("/query/suggest_empty_room", function (address) {
        connect_room(address, default_type);
    });
}

function random_room() {
    $("<div>").load("/query/suggest_random_room", function (address) {
        connect_room(address, "camera");
    });
}
