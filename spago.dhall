{ name = "web3-tests"
, dependencies =
  [ "effect"
  , "either"
  , "maybe"
  , "newtype"
  , "partial"
  , "prelude"
  , "profunctor-lenses"
  , "tagged"
  , "web3"
  , "web3-generator"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
