{ name = "aff-coroutines"
, dependencies =
  [ "aff"
  , "avar"
  , "console"
  , "coroutines"
  , "effect"
  , "either"
  , "freet"
  , "maybe"
  , "newtype"
  , "prelude"
  , "transformers"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
