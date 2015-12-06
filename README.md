# Hajong - Riichi mahjong suite

See http://funktionaali.com/2014-05-04-Riichi.html

> for i in ../../../hajong-client-web/dist/images/*; do ln -s $i .; done

## Installation

> npm install elm
> export PATH=./node_modules/.bin/:$PATH

# hajong-client-web Web Client

Requires elm 0.15

compile with `hajong-client-web/build.sh`


# Technical notes

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
