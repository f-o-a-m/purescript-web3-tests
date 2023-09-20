module Web3Spec.Live.ContractUtils where

import Prelude

import Chanterelle.Deploy (deployContract)
import Chanterelle.Test (assertWeb3)
import Chanterelle.Types.Deploy (ContractConfig, DeployConfig(..), DeployM)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Data.Array.NonEmpty as NAE
import Data.Lens ((?~))
import Data.Maybe (fromJust)
import Data.Newtype (wrap, unwrap)
import Data.Traversable (intercalate)
import Effect.Aff (Aff, Milliseconds(..), delay)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class.Console as C
import Network.Ethereum.Core.BigNumber (decimal, fromStringAs)
import Network.Ethereum.Web3 (Address, BigNumber, BlockNumber, HexString, Provider, TransactionOptions, _from, _gas, defaultTransactionOptions, mkHexString)
import Network.Ethereum.Web3.Api as Api
import Network.Ethereum.Web3.Types (NoPay)
import Partial.Unsafe (unsafePartial)
import Test.Spec (ComputationType(..), SpecT, hoistSpec)

type Logger m = String -> m Unit

go :: SpecT (ReaderT (Logger Aff) Aff) Unit Aff ~> SpecT Aff Unit Aff
go =
  hoistSpec identity \cType m ->
    let
      prefix = case cType of
        CleanUpWithContext n -> intercalate " > " n <> " (afterAll) "
        TestWithName n -> intercalate " > " $ NAE.toArray n
    in
      runReaderT m \logMsg -> C.log $ prefix <> "| " <> logMsg

hangOutTillBlock
  :: forall m
   . MonadAff m
  => Provider
  -> Logger m
  -> BlockNumber
  -> m Unit
hangOutTillBlock provider logger bn = do
  bn' <- liftAff $ assertWeb3 provider Api.eth_blockNumber
  logger $ "Current block number : " <> show bn'
  when (bn' < bn) do
    liftAff $ delay (Milliseconds 1000.0)
    hangOutTillBlock provider logger bn

awaitNextBlock
  :: forall m
   . MonadAff m
  => Provider
  -> Logger m
  -> m Unit
awaitNextBlock provider logger = do
  n <- liftAff $ assertWeb3 provider Api.eth_blockNumber
  let
    next = wrap $ one + unwrap n
  logger $ "Awaiting block number " <> show next
  hangOutTillBlock provider logger next

mkHexString'
  :: String
  -> HexString
mkHexString' hx = unsafePartial fromJust $ mkHexString hx

defaultTestTxOptions :: TransactionOptions NoPay
defaultTestTxOptions = defaultTransactionOptions # _gas ?~ bigGasLimit

bigGasLimit :: BigNumber
bigGasLimit = unsafePartial fromJust $ fromStringAs decimal "4712388"

deployScript :: forall a. ContractConfig a -> DeployM { deployAddress :: Address, primaryAccount :: Address }
deployScript contractConfig = do
  DeployConfig { primaryAccount } <- ask
  let txOpts = defaultTestTxOptions # _from ?~ primaryAccount
  { deployAddress } <- deployContract txOpts contractConfig
  pure { deployAddress, primaryAccount }

nodeUrl :: String
nodeUrl = "http://localhost:8545"
