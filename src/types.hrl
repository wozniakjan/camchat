-record(room, {room_id, user_list, default_stream = <<"camera">>, key = <<"">>}).
-record(user, {connection_id, room_id, user_id, username, browser_token}).
