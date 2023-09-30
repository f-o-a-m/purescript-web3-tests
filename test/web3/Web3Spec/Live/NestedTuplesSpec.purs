module Web3Spec.Live.NestedTuplesSpec (spec) where

import Prelude

import Chanterelle.Test (assertWeb3, buildTestConfig)
import Chanterelle.Utils (pollTransactionReceipt)
import Contract.NestedTuples as NestedTuples
import Data.Either (Either(..))
import Data.Lens ((?~))
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Network.Ethereum.Web3 (ChainCursor(..), TransactionReceipt(..), TransactionStatus(..), _from, _to)
import Network.Ethereum.Web3.Solidity (Tuple2(..))
import Network.Ethereum.Web3.Solidity.Bytes as BytesN
import Network.Ethereum.Web3.Solidity.UInt as UIntN
import Test.QuickCheck (arbitrary)
import Test.QuickCheck.Gen (randomSampleOne)
import Test.QuickCheck.Gen as Gen
import Test.Spec (SpecT, beforeAll, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))
import Web3Spec.Encoding.ContainersSpec (BMPString(..))
import Web3Spec.Live.ContractConfig as ContractConfig
import Web3Spec.Live.ContractUtils (defaultTestTxOptions, deployScript, nodeUrl)

spec :: SpecT Aff Unit Aff Unit
spec =
  describe "Nested Tuple"
    $ beforeAll (buildTestConfig nodeUrl 60 $ deployScript ContractConfig.nestedTupleCfg)
    $ it "Can get and set a value"
    $ \cfg -> do
        arg <- liftEffect $ randomSampleOne do
          a1 <- UIntN.generator (Proxy @256)
          BMPString a2 <- arbitrary @BMPString
          let a = Tuple2 a1 a2
          b1 <- Gen.arrayOf ((\(BMPString s) -> s) <$> arbitrary @BMPString)
          b2 <- BytesN.generator $ Proxy @32
          let b = Tuple2 b1 b2
          pure { a, b }

        let

          txOpts = defaultTestTxOptions
            # _from ?~ cfg.primaryAccount
            # _to ?~ cfg.deployAddress

        hash <- assertWeb3 cfg.provider $ NestedTuples.update txOpts arg
        TransactionReceipt { status } <- liftAff $ pollTransactionReceipt hash cfg.provider
        status `shouldEqual` Succeeded
        eRes <- assertWeb3 cfg.provider $ NestedTuples.c txOpts Latest
        eRes `shouldEqual` Right (Tuple2 arg.a arg.b)
