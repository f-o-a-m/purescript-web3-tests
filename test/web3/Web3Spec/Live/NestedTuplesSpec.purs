module Web3Spec.Live.NestedTuplesSpec (spec) where

import Prelude

import Chanterelle.Test (assertWeb3, buildTestConfig, takeEvent)
import Chanterelle.Utils (pollTransactionReceipt)
import Contract.NestedTuples as NestedTuples
import Data.Either (Either(..))
import Data.Lens ((?~))
import Data.Maybe (fromJust)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Network.Ethereum.Web3 (ChainCursor(..), HexString, TransactionReceipt(..), TransactionStatus(..), Web3, _from, _to, fromInt)
import Network.Ethereum.Web3.Solidity (Tuple2(..))
import Network.Ethereum.Web3.Solidity.Bytes as BytesN
import Network.Ethereum.Web3.Solidity.UInt as UIntN
import Partial.Unsafe (unsafePartial)
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
        let
          mkArg = liftEffect $ randomSampleOne do
            a1 <- UIntN.generator (Proxy @256)
            BMPString a2 <- arbitrary @BMPString
            let a = { a1, a2 }
            b1 <- Gen.arrayOf ((\(BMPString s) -> s) <$> arbitrary @BMPString)
            b2 <- BytesN.generator $ Proxy @32
            let b = { b1, b2 }
            pure { a, b }

        let
          txOpts = defaultTestTxOptions
            # _from ?~ cfg.primaryAccount
            # _to ?~ cfg.deployAddress

        let
          action arg = NestedTuples.update txOpts arg

        arg1 <- mkArg
        Tuple _ (NestedTuples.Update { x, y, z }) <-
          assertWeb3 cfg.provider $
            takeEvent
              (Proxy @NestedTuples.Update)
              cfg.deployAddress
              (action arg1)
        x `shouldEqual` arg1.a
        y `shouldEqual` arg1.b
        let p = { a: { a1: x.a1, a2: x.a2 }, b: { b1: y.b1, b2: y.b2 } }
        z `shouldEqual` [ p ]

        arg2 <- mkArg
        Tuple _ (NestedTuples.Update { x, y, z }) <-
          assertWeb3 cfg.provider $
            takeEvent
              (Proxy @NestedTuples.Update)
              cfg.deployAddress
              (action arg2)
        x `shouldEqual` arg1.a
        y `shouldEqual` arg1.b
        let q = { a: { a1: x.a1, a2: x.a2 }, b: { b1: y.b1, b2: y.b2 } }
        z `shouldEqual` [ q, p ]

        eRes1 <- assertWeb3 cfg.provider $ NestedTuples.cs txOpts Latest (mkUInt zero)
        eRes1 `shouldEqual` Right q

        eRes2 <- assertWeb3 cfg.provider $ NestedTuples.cs txOpts Latest (mkUInt one)
        eRes2 `shouldEqual` Right p

mkUInt :: Int -> UIntN.UIntN 256
mkUInt n = unsafePartial $ fromJust $ UIntN.uIntNFromBigNumber (Proxy @256) $ fromInt n
