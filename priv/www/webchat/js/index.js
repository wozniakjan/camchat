var slow_show_lock = false;

function join_room(default_type) {
    var address = "";
    if(default_type == "camera"){
        address = $("#chat_with_friends .room_name").val();
        save_key($("#chat_with_friends .key"));
    } else if (default_type == "screen") {
        address = $("#share_screen .room_name").val();
        save_key($("#share_screen .key"));
    }
    connect_room(address, "camera");
}

function save_key(div) {
    var key = div.val();
    if(key != "set key..." && key != ""){
        sessionStorage.setItem("room_key", key);
    } else {
        sessionStorage.removeItem("room_key");
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

function is_scroll_visible(elem) {
    var view_top = $(window).scrollTop();
    var view_bottom = view_top + $(window).height();

    var elem_top = $(elem).offset().top;
    var elem_bottom = elem_top + $(elem).height();

    return ((elem_bottom >= view_top) && (elem_top <= view_bottom)
            && (elem_bottom <= view_bottom) &&  (elem_top >= view_top) );
}

function slow_show(elem, message, i) {
    if(i < message.length) {
        elem.val(elem.val() + message[i++]);
        setTimeout(function() {slow_show(elem, message, i)},100);
    } else {
        slow_show_lock = false;
    }
}

function write_random(elem_input) {
    if(elem_input.val() == '' && !elem_input.hasClass("active")){
        $("<div>").load("/query/suggest_empty_room", 
                function (rand){
                    if(!slow_show_lock){
                        slow_show_lock = true;
                        slow_show(elem_input,rand,0);
                    }
                })
    }
}


$(document).ready (function(){
    var room_name = $(".room_name");
    room_name.blur(function() {
        $(this).removeClass("active"); 
        write_random($(this));
    })
    room_name.focus(function() {$(this).addClass("active"), $(this).val('');});
    room_name.change(function() {this.value = this.value.replace(/\W/g, '');});
    var key = $(".key");
    key.blur(function() {if ($(this).val() == '') {$(this).val('set key...');}});
    key.focus(function() {if ($(this).val('set key...')) {$(this).val('');}});
    key.change(function() {$(this).val($(this).val().replace(/\W/g, ''))});
    
    $(window).scroll(function(){
        $(".room_name").each(function() {
            if(is_scroll_visible($(this))){
                write_random($(this));
            }
        })
    });
});

