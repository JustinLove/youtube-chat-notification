## Youtube Chat Notification

Shows HTML Notifications when Youtube chat event occur, and can play a sound if chat had been idle for a bit.

## Limitations

- Don't have a good way to refresh oauth tokens in a client only app, so polling rate will limited once oauth token expires. We need an oauth login to get live streams, but can later poll the chat without authentication, just slower.
- Browser notifications do not work from local files or in incognito windows (at least in Chrome)

## Compiling

- [Elm](http://elm-lang.org/) to build from source

A [Youtube api-key and client-id](https://developers.google.com/youtube/v3/live/registering_an_application) is required to make API calls. This is defined in `src/YoutubeId.elm`. This file is not part of of the repo, but `src/YoutubeId.elm.example` can be copied and edited to provide your information. Proxy url is related to auth refresh, which is not currently in use.

My build command:

> `elm-make src/YoutubeChatNotification.elm --output public/youtube-chat-notification.js`

`bin/monitor.bat` has a command using the [watch](https://www.npmjs.com/package/watch) CLI


## Credits

- Alert sound: ['Typewriter ding_near_mono.wav'](https://freesound.org/people/_stubb/sounds/406243/) by `_stubb`
- Icons: [IcoMoon - Free](https://icomoon.io/#icons-icomoon) ([CC BY 4.0](http://creativecommons.org/licenses/by/4.0/))
