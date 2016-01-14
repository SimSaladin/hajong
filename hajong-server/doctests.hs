import Test.DocTest

main = doctest
    [ "-XConstraintKinds", "-XFlexibleContexts", "-XFlexibleInstances"
    , "-XOverloadedStrings", "-XTemplateHaskell", "-XDeriveDataTypeable"
    , "-XLambdaCase", "-XRankNTypes", "-XNoImplicitPrelude"
    , "-XRecordWildCards"
    , "-XGeneralizedNewtypeDeriving"
    , "-XStandaloneDeriving"
    , "-hide-package control-monad-free"
    , "-isrc"
    , "src/Mahjong/Hand/Fu.hs"
    , "src/Mahjong/Hand/Value.hs"
    , "src/Mahjong/Kyoku.hs" ]
