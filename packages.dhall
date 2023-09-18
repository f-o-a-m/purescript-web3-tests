let upstream =
      https://raw.githubusercontent.com/f-o-a-m/package-sets/purs-0.15/purs-0.15.7-web3.dhall
        sha256:ce57fd949b7cd331d7c61ff45283e35983dd5797b3f17616dd69f8bc06f54784

  with eth-core = ./purescript-eth-core/spago.dhall as Location
  with web3 = ./purescript-web3/spago.dhall as Location
  with web3-generator = ./purescript-web3-generator/spago.dhall as Location
  with solc = ./purescript-solc/spago.dhall as Location
  with chanterelle = ./chanterelle/spago.dhall as Location
  with js-bigints.version = "v2.2.0"


in  upstream
