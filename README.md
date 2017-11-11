camchat
================================================================================

Camchat is implementation of signaling server for HTML5 based video chat with
support for conference rooms as well as front-end client side. Currently it is
recommended to use Chrome, Opera or Firefox web browser for camera chat and
only Chrome for screen sharing feature, once the development in the field 
stabilizes a bit more, there is a plan for supporting features on all relevant
browsers.

### Intro
There is up and running camchat on heroku.com 
    
    https://camchat.herokuapp.com

Once you enter the room, you should see something similar to this

![Alt text](https://raw.githubusercontent.com/wozniakjan/camchat/master/priv/screenshots/main.png "Example picture")

Some of the interesting features include:

* **screen sharing** - you can turn it on via `search -> Audio & Video Settings -> stream`
        currently supported only on chrome v36 via `chrome://flags/#enable-usermedia-screen-capture`
* **director's cut** - rooms are multi-user, which is usefull for conferences, 
        however, multiple, user means less space for everyone on the screen.
        This feature will select 'loudest talking' peer and display him on
        main screen (currently via sound signal energy computation).

### Implementation
This project includes both back-end signaling and front-end app. Backend is
implemented in Erlang, uses Cowboy web server and tries to adopt OTP principles.
Frontend is jQuery and few jQuery plugins.

more information soon to come, feel free to contact me with any questions and comments.
