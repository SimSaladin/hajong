import Test.DocTest

main = doctest
    [ "-XConstraintKinds", "-XFlexibleContexts", "-XFlexibleInstances"
    , "-XOverloadedStrings", "-XTemplateHaskell", "-XDeriveDataTypeable"
    , "-XLambdaCase", "-XRankNTypes", "-XNoImplicitPrelude"
    , "-XRecordWildCards"
    , "-XStandaloneDeriving"
    , "-isrc"
    , "src/Mahjong/Hand/Value.hs", "src/Mahjong/Kyoku.hs" ]
