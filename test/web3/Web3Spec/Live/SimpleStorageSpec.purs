module Web3Spec.Live.SimpleStorageSpec (spec) where

import Prelude

import Chanterelle.Test (assertWeb3, buildTestConfig, takeEvent)
import Contract.SimpleStorage as SimpleStorage
import Data.Either (isRight)
import Data.Lens ((?~))
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Network.Ethereum.Web3 (ChainCursor(..), _from, _to, runWeb3)
import Network.Ethereum.Web3.Api as Api
import Network.Ethereum.Web3.Solidity.UInt as UIntN
import Test.QuickCheck.Gen (randomSampleOne)
import Test.Spec (SpecT, beforeAll, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import Type.Proxy (Proxy(..))
import Web3Spec.Live.ContractConfig as ContractConfig
import Web3Spec.Live.ContractUtils (defaultTestTxOptions, deployScript, nodeUrl)

spec :: SpecT Aff Unit Aff Unit
spec =
  describe "Simple Storage"
    $ beforeAll (buildTestConfig nodeUrl 60 $ deployScript ContractConfig.simpleStorageCfg)
    $ it "Can get and set a simple UInt with events"
    $ \simpleStorageCfg -> do
        newCount <- liftEffect $ randomSampleOne (UIntN.generator (Proxy @256))
        let
          { deployAddress: simpleStorageAddress, primaryAccount: userAddress, provider } = simpleStorageCfg

          txOpts =
            defaultTestTxOptions # _from ?~ userAddress
              # _to
                  ?~ simpleStorageAddress

          setCountTx = SimpleStorage.setCount txOpts { _count: newCount }
        Tuple _ (SimpleStorage.CountSet { _count }) <-
          assertWeb3 provider
            $ takeEvent (Proxy :: Proxy SimpleStorage.CountSet) simpleStorageAddress setCountTx
        _count `shouldEqual` newCount
        eRes' <- runWeb3 provider $ Api.eth_getStorageAt simpleStorageAddress zero Latest
        eRes' `shouldSatisfy` isRight
