{ name = "web3-tests"
, dependencies =
  [ "effect"
  , "prelude"
  , "web3-generator"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
