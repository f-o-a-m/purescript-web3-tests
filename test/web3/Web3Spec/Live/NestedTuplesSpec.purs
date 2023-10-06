module Web3Spec.Live.NestedTuplesSpec (spec) where

import Prelude

import Chanterelle.Test (assertWeb3, buildTestConfig, takeEvent)
import Contract.NestedTuples as NestedTuples
import Data.Either (Either(..))
import Data.Lens ((?~))
import Data.Maybe (fromJust)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Network.Ethereum.Web3 (ChainCursor(..), _from, _to, fromInt)
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

        { res: p } <- do
          arg <- mkArg
          Tuple _ (NestedTuples.Update { x, y, z }) <-
            assertWeb3 cfg.provider $
              takeEvent
                (Proxy @NestedTuples.Update)
                cfg.deployAddress
                (action arg >>= \txHash -> (Console.log "LISTEN") *> pure txHash)
          x `shouldEqual` arg.a
          y `shouldEqual` arg.b
          let p = { a: { a1: x.a1, a2: x.a2 }, b: { b1: y.b1, b2: y.b2 } }
          z `shouldEqual` [ p ]
          pure { arg, res: p }

        { res: q } <- do
          arg <- mkArg
          Tuple _ (NestedTuples.Update { x, y, z }) <-
            assertWeb3 cfg.provider $
              takeEvent
                (Proxy @NestedTuples.Update)
                cfg.deployAddress
                (action arg)
          x `shouldEqual` arg.a
          y `shouldEqual` arg.b
          let q = { a: { a1: x.a1, a2: x.a2 }, b: { b1: y.b1, b2: y.b2 } }
          z `shouldEqual` [ p, q ]
          pure { arg, res: q }

        eRes1 <- assertWeb3 cfg.provider $ NestedTuples.cs txOpts Latest (mkUInt zero)
        eRes1 `shouldEqual` Right p

        eRes2 <- assertWeb3 cfg.provider $ NestedTuples.cs txOpts Latest (mkUInt one)
        eRes2 `shouldEqual` Right q

mkUInt :: Int -> UIntN.UIntN 256
mkUInt n = unsafePartial $ fromJust $ UIntN.uIntNFromBigNumber (Proxy @256) $ fromInt n
