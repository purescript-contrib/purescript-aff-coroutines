{ name = "aff-coroutines"
, dependencies =
  [ "aff", "avar", "console", "coroutines", "effect", "psci-support" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
