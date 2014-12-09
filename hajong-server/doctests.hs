import Test.DocTest

main = doctest
    [ "-XConstraintKinds", "-XFlexibleContexts", "-XFlexibleInstances"
    , "-XOverloadedStrings", "-XTemplateHaskell", "-XDeriveDataTypeable"
    , "-XLambdaCase", "-XRankNTypes", "-XNoImplicitPrelude"
    , "-isrc"
    , "src/Mahjong/Deal.hs" ]
