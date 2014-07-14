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

function is_scroll_visible(elem)
{
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
    }
}

function write_random(elem_input) {
    if(elem_input.val() == '' && !elem_input.hasClass("active")){
        $("<div>").load("/query/suggest_random_room", 
                function (rand){slow_show(elem_input,rand,0);})
    }
}

$(window).scroll(function(){
    $(".room_name").each(function() {
        if(is_scroll_visible($(this))){
            write_random($(this));
        }
    })
});

$(document).ready (function(){
    var room_name = $(".room_name");
    room_name.blur(function() {
        $(this).removeClass("active"); 
        write_random($(this));
    })
    room_name.focus(function() {$(this).addClass("active"), $(this).val('');});
    room_name.change(function() {this.value = this.value.replace(/\W/g, '');});
    var password = $(".password");
    password.blur(function() {if ($(this).val('')) {$(this).val('set..');}});
    password.focus(function() {if ($(this).val('set..')) {$(this).val('');}});
    password.change(function() {$(this).val($(this).val().replace(/\W/g, ''))});
});

