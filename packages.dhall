let upstream =
      https://raw.githubusercontent.com/f-o-a-m/package-sets/purs-0.15-web3/purs-0.15.7-web3.dhall
        sha256:cb35bdebefab6fd0d9b0a09b1f461cd8e053509b12ee17099d9324287d20f1f5


  with eth-core = ./purescript-eth-core/spago.dhall as Location
  with web3 = ./purescript-web3/spago.dhall as Location
  with web3-generator = ./purescript-web3-generator/spago.dhall as Location
  with solc = ./purescript-solc/spago.dhall as Location
  with chanterelle = ./chanterelle/spago.dhall as Location


in  upstream
