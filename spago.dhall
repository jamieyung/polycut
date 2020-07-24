{ name = "polysub"
, dependencies =
  [ "console"
  , "debug"
  , "effect"
  , "halogen"
  , "numbers"
  , "profunctor-lenses"
  , "psci-support"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
