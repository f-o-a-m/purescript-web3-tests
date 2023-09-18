# purescript-web3-tests

This repo is the closest thing you can get to a monorepo in spago (unfortunately as of right now you cannot manage multiple packes in one repo with spago).

Previously things were extremely awkward if you needed to make a release of the ps-web3-* suite. It should be easier to write/run tests across the different repos from here.

The `Live` tests from purescript-web3 have been ported to this repo, but as of now we are not using chanterelle to generate the FFI for those contracts. 
Instead we are invoking `solc` and `ps-web3-generator` directly.

You can run the tests locally via

```
> docker run -d -p 8545:8545 -e ACCOUNTS_TO_CREATE=10 foamspace/cliquebait:v1.9.12
> npm install
> npm run build 
> npm run chanterelle-build
> npm run test

```
