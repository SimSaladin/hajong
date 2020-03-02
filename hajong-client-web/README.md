# hajong-client-web

```
src/
    Main.elm   # main
    Mockup.elm # another main
    ViewingGame.elm # the third main (static game views)

    Util.elm

    Game.elm        # bloaty game view: refactor to smaller modules.
    Lounge.elm      # Lounge view
    MsgDialog       # a component
    View.elm        # some general shit that should be in different, better-named modules

    # communication with hajong-server
    GameTypes.elm
    Events.elm
    JSON.elm

    # communication with hajong-site
    PlayerInfo.elm

```
