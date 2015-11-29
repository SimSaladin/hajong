# Hajong - Riichi mahjong suite

See http://funktionaali.com/2014-05-04-Riichi.html

> for i in ../../../hajong-client-web/dist/images/*; do ln -s $i .; done

## Installation

> npm install elm
> export PATH=./node_modules/.bin/:$PATH

# hajong-client-web Web Client

Requires elm 0.15

compile with

```
hajong/hajong-client-web $ elm make Main.elm
```


# Technical notes

## Authentication scheme

??

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
