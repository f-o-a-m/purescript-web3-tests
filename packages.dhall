let upstream =
      https://raw.githubusercontent.com/f-o-a-m/package-sets/purs-0.15/purs-0.15.7-web3.dhall
        sha256:2d868539460c47c2bf5ecf4c6b68c3ea3162849f2da9cd3f263b740299448d8f

  with eth-core = ./purescript-eth-core/spago.dhall as Location
  with web3 = ./purescript-web3/spago.dhall as Location
  with web3-generator = ./purescript-web3-generator/spago.dhall as Location
  with chanterelle = ./chanterelle/spago.dhall as Location
  with js-bigints.version = "18dc9a101de22b38738802bacf90249f8d7be8bb"


let additions = 

  { solc =
    { dependencies =
      [ "aff"
      , "argonaut"
      , "argonaut-codecs"
      , "arrays"
      , "bifunctors"
      , "control"
      , "effect"
      , "either"
      , "eth-core"
      , "foldable-traversable"
      , "foreign-object"
      , "functions"
      , "integers"
      , "maybe"
      , "newtype"
      , "node-path"
      , "prelude"
      , "strings"
      , "transformers"
      , "tuples"
      ]
    , repo =
        "https://github.com/f-o-a-m/purescript-solc.git"
    , version =
        "d6e4f3be2f249d9a464bf9bbb6ca247bc24a759e"
    }
  }


in  upstream // additions
