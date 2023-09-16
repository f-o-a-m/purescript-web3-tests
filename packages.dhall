let upstream =
      https://raw.githubusercontent.com/f-o-a-m/package-sets/purs-0.15/purs-0.15.7-web3.dhall
        sha256:ce57fd949b7cd331d7c61ff45283e35983dd5797b3f17616dd69f8bc06f54784

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
