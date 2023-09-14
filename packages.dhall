let upstream =
      https://raw.githubusercontent.com/f-o-a-m/package-sets/purs-0.15/purs-0.15.7-web3.dhall
        sha256:2d868539460c47c2bf5ecf4c6b68c3ea3162849f2da9cd3f263b740299448d8f

  with eth-core = ./purescript-eth-core/spago.dhall as Location
  with web3 = ./purescript-web3/spago.dhall as Location
  with web3-generator = ./purescript-web3-generator/spago.dhall as Location
  with js-bigints.repo = "https://github.com/martyall/purescript-js-bigints"
  with js-bigints.version = "fix-fromStringAs"


in  upstream
