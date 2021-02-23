{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "my-project"
, dependencies =
    [ "argonaut"
    , "argonaut-generic"
    , "codec-argonaut"
    , "console"
    , "control"
    , "corefn"
    , "effect"
    , "either"
    , "node-fs"
    , "node-fs-aff"
    , "psci-support"
    , "validation"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
