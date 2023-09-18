module Web3Spec.Live.MockERC20Spec where

import Prelude

import Chanterelle.Test (buildTestConfig)
import Contract.MockERC20 as MockERC20
import Data.Lens ((?~))
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Network.Ethereum.Web3 (_from, _to)
import Network.Ethereum.Web3.Solidity.UInt as UIntN
import Test.QuickCheck.Gen (randomSampleOne)
import Test.Spec (SpecT, beforeAll, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))
import Web3Spec.Live.ContractConfig as ContractConfig
import Web3Spec.Live.ContractUtils (deploy, nodeUrl, takeEvent)
import Web3Spec.Live.Utils (assertWeb3, defaultTestTxOptions, nullAddress)

spec :: SpecT Aff Unit Aff Unit
spec =
  describe "MockERC20"
    $ beforeAll (buildTestConfig nodeUrl 60 $ deploy ContractConfig.mockERC20Cfg)
    $ it "can make a transfer"
    $ \cfg -> do
        amount <- liftEffect $ randomSampleOne (UIntN.generator (Proxy @256))
        let
          { deployAddress: mockERC20Address, primaryAccount: userAddress, provider } = cfg

          recipient = nullAddress

          txOptions =
            defaultTestTxOptions # _from ?~ userAddress
              # _to
                  ?~ mockERC20Address

          transferAction = MockERC20.transfer txOptions { to: recipient, amount: amount }
        Tuple _ (MockERC20.Transfer tfr) <-
          assertWeb3 provider
            $ takeEvent (Proxy :: Proxy MockERC20.Transfer) mockERC20Address transferAction
        tfr.amount `shouldEqual` amount
