function join_room(default_type) {
    sessionStorage.setItem("default_media_type",default_type)
    address = $("#room_name").val();
    console.log(address);
    window.location.href = address;
}

function empty_room() {
    address = "empty";
    window.location.href = address;
}

function random_room() {
    address = "random";
    window.location.href = address;
}
