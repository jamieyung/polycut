{ name = "polysub"
, dependencies =
  [ "console"
  , "debug"
  , "effect"
  , "halogen"
  , "profunctor-lenses"
  , "psci-support"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
