app_params="-s camchat"
if [ -n "$COOKIE" ]; then
    app_params="$app_params -setcookie $COOKIE"
fi
if [ -n "$NODE" ]; then
    app_params="$app_params -name $NODE"
fi

erl -pa ebin deps/*/ebin $app_params #-noshell -noinput
