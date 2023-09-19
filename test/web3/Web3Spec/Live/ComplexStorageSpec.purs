module Web3Spec.Live.ComplexStorageSpec (spec) where

import Prelude

import Chanterelle.Test (assertWeb3, buildTestConfig, takeEvent)
import Contract.ComplexStorage as ComplexStorage
import Data.Lens ((?~))
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Network.Ethereum.Web3 (_from, _to)
import Network.Ethereum.Web3.Solidity.Bytes as BytesN
import Network.Ethereum.Web3.Solidity.Int as IntN
import Network.Ethereum.Web3.Solidity.UInt as UIntN
import Network.Ethereum.Web3.Solidity.Vector as Vector
import Test.QuickCheck (arbitrary)
import Test.QuickCheck.Gen (arrayOf, randomSampleOne)
import Test.Spec (SpecT, beforeAll, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))
import Web3Spec.Encoding.ContainersSpec (BMPString(..))
import Web3Spec.Live.ContractConfig as ContractConfig
import Web3Spec.Live.ContractUtils (defaultTestTxOptions, deployScript, nodeUrl)

spec :: SpecT Aff Unit Aff Unit
spec =
  describe "Complex Storage"
    $ beforeAll (buildTestConfig nodeUrl 60 $ deployScript ContractConfig.complexStorageCfg)
    $ it "Can encode and decode complex objects to / from a smart contract"
    $ \{ deployAddress: complexStorageAddress, primaryAccount: userAddress, provider } -> do
        arg <- liftEffect $ do
          uint <- randomSampleOne $ UIntN.generator $ Proxy @256
          int <- randomSampleOne $ IntN.generator $ Proxy @256
          bool <- randomSampleOne (arbitrary @Boolean)
          int224 <- randomSampleOne $ IntN.generator $ Proxy @224
          bools <- randomSampleOne (Vector.generator (Proxy @2) (arbitrary @Boolean))
          ints <- randomSampleOne (arrayOf (IntN.generator $ Proxy @256))
          BMPString string <- randomSampleOne (arbitrary @BMPString)
          bytes16 <- randomSampleOne (BytesN.generator $ Proxy @16)
          bytes2s <- randomSampleOne (arrayOf $ Vector.generator (Proxy @4) (BytesN.generator (Proxy @2)))
          pure $
            { _uintVal: uint
            , _intVal: int
            , _boolVal: bool
            , _int224Val: int224
            , _boolVectorVal: bools
            , _intListVal: ints
            , _stringVal: string
            , _bytes16Val: bytes16
            , _bytes2VectorListVal: bytes2s
            }
        let
          txOptions =
            defaultTestTxOptions
              # _from ?~ userAddress
              # _to ?~ complexStorageAddress

          setValsAction = ComplexStorage.setValues txOptions arg
        pure unit
        Tuple _ _event <- liftAff
          $ assertWeb3 provider
          $ takeEvent (Proxy :: Proxy ComplexStorage.ValsSet) complexStorageAddress setValsAction
        _event `shouldEqual` ComplexStorage.ValsSet { a: arg._uintVal, b: arg._intVal, c: arg._boolVal, d: arg._int224Val, e: arg._boolVectorVal, f: arg._intListVal, g: arg._stringVal, h: arg._bytes16Val, i: arg._bytes2VectorListVal }
