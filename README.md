# Hajong - Riichi mahjong suite

See http://funktionaali.com/2014-05-04-Riichi.html

Installation
============

This section outlines development setup.

1. Build the web play client
   1. Install Elm-Platform 0.16
   2. `hajong-client-web/build.sh`
2. Build game server and the site
   1. Install `stack`
   2. `./build.sh`
   3. Install and start `postgresql`, allow local logins without password.
   4. Create user and database for the development site
      ```
      sudo -u postgres createuser hajong-site
      sudo -u postgres createdb -O hajong-site hajong-site
      ```
3. Start game server in `hajong-site` directory
   ```
   cd hajong-site
   stack exec hajong-server
   ```
   By default, logs go to `./logs`, game server state is stored at `./state` and
   `hajong-server` reads configuration at `./config/settings.yml`.
4. In another terminal, start the site in development mode
   ```
   stack install yesod-bin
   cd hajong-site
   stack exec yesod devel
   ```

All set! To verify a new account in the development app, do
```
psql -u hajong-site
> update "user" set verified = true;
```

Contributing
============

To make changes and test them in...

* **Web client**: The elm code lives under `hajong-client-web`. Use
    `hajong-client-web/build.sh` to test after changes. The new client is
    immediately active in the development app.
  
  If you wish to view the client in some predefined game states, for UI etc.
  there is a `Mockup.elm` which you may freely modify and view via
  `elm-reactor`.
* **Website**: powered by yesod, so normal `yesod devel` stuff applies.
* **Game server**: lives under `hajong-server`. Run tests with `stack test` in
  that directory.

Pull requests are welcome. If you are looking for something to contribute, take
a look at the TODO file; especially the tasks related to the UI hang low. I
recommend using the mockup described above to see the visual changes.

Technical notes
===============

## 

*This section might be slightly out of date*

## Authentication scheme

Authentication happens first in to the yesod application either via
account/password or facebook. When loading the play screen we fetch game server
ident+token to localstorage indirectly from the game server (via the yesod app).
On the play screen we identify ourselves with the ident+token combo direct to
the game server via websockets.  We clear the token when a page load is made
where we are not logged in to the yesod application.


## About lens naming

Prefixes:

`Game       { _g }` (Hajong.Server)  
`ServerState { _se }`  
`WorkerData { _w }` (Hajong.Worker)  

`Value      { _va }` (Mahjong.Hand.Value)  
`Hand       { _h }` (todo)  
`Discard    { _dc }`  
`ValueHand  { _vh }`  
`Deal       { _p, _s }`

## Random notes

> npm install elm
> export PATH=./node_modules/.bin/:$PATH

> for i in ../../../hajong-client-web/dist/images/*; do ln -s $i .; done
