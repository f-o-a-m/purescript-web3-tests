module Web3Spec.Live.ComplexStorageSpec (spec) where

import Prelude

import Contract.ComplexStorage as ComplexStorage
import Data.Lens ((?~))
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console as C
import Network.Ethereum.Web3 (Provider, Value, Wei, _data, _from, _to, _value, mkValue)
import Network.Ethereum.Web3.Api as Api
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
import Web3Spec.Live.Code.ComplexStorage as ComplexStorageCode
import Web3Spec.Live.ContractUtils (deployContract, takeEvent)
import Web3Spec.Live.Utils (assertWeb3, defaultTestTxOptions)

spec :: Provider -> SpecT Aff Unit Aff Unit
spec provider =
  describe "Complex Storage"
    $ beforeAll
        ( deployContract provider C.log "ComplexStorage"
            $ \txOpts ->
                Api.eth_sendTransaction $ txOpts # _data ?~ ComplexStorageCode.deployBytecode
                  # _value
                      ?~ (mkValue zero :: Value Wei)
        )
    $ it "Can encode and decode complex objects to / from a smart contract"
    $ \complexStorageCfg -> do
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
          { contractAddress: complexStorageAddress, userAddress } = complexStorageCfg

          txOptions =
            defaultTestTxOptions # _from ?~ userAddress
              # _to
                  ?~ complexStorageAddress

          setValsAction = ComplexStorage.setValues txOptions arg
        pure unit
        Tuple _ _event <-
          assertWeb3 provider
            $ takeEvent (Proxy :: Proxy ComplexStorage.ValsSet) complexStorageAddress setValsAction
        _event `shouldEqual` ComplexStorage.ValsSet { a: arg._uintVal, b: arg._intVal, c: arg._boolVal, d: arg._int224Val, e: arg._boolVectorVal, f: arg._intListVal, g: arg._stringVal, h: arg._bytes16Val, i: arg._bytes2VectorListVal }
