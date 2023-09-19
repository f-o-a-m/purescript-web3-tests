module Web3Spec.Live.ContractUtils where

import Prelude

import Chanterelle.Types.Deploy (ContractConfig, DeployConfig(..), DeployM)
import Chanterelle.Internal.Deploy (deployContract)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Data.Array.NonEmpty as NAE
import Data.Either (Either(..))
import Data.Lens ((?~))
import Data.Maybe (fromJust)
import Data.Newtype (wrap, unwrap)
import Data.Traversable (intercalate)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff, Milliseconds(..), Fiber, joinFiber, delay)
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class.Console as C
import Network.Ethereum.Core.BigNumber (decimal, fromStringAs)
import Network.Ethereum.Core.Signatures (mkAddress)
import Network.Ethereum.Web3 (class EventFilter, Address, BigNumber, BlockNumber, CallError, EventAction(..), HexString, Provider, TransactionOptions, Web3, Web3Error, _from, _gas, defaultTransactionOptions, event, eventFilter, forkWeb3', mkHexString)
import Network.Ethereum.Web3.Api as Api
import Network.Ethereum.Web3.Solidity (class DecodeEvent)
import Network.Ethereum.Web3.Types (NoPay)
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
import Test.Spec (ComputationType(..), SpecT, hoistSpec)
import Type.Proxy (Proxy)
import Web3Spec.Live.Utils (assertWeb3)

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

-- | Run a `Web3` action which will dispatch a single event, wait for the event,
-- | then return the action's result and the event.
takeEvent
  :: forall a ev i ni
   . DecodeEvent i ni ev
  => Show ev
  => EventFilter ev
  => Proxy ev
  -> Address
  -> Web3 a
  -> Web3 (Tuple a ev)
takeEvent prx addrs web3Action = do
  var <- liftAff AVar.empty
  _ <-
    forkWeb3' do
      event (eventFilter prx addrs)
        $ \e -> do
            _ <- liftAff $ AVar.put e var
            pure TerminateEvent
  efRes <- web3Action
  event <- liftAff $ AVar.take var
  pure $ Tuple efRes event

assertStorageCall
  :: forall m a
   . MonadAff m
  => Provider
  -> Web3 (Either CallError a)
  -> m a
assertStorageCall p f =
  liftAff do
    eRes <- assertWeb3 p f
    case eRes of
      Right x -> pure x
      Left err -> unsafeCrashWith $ "expected Right in `assertStorageCall`, got error" <> show err

hangOutTillBlock
  :: forall m
   . MonadAff m
  => Provider
  -> Logger m
  -> BlockNumber
  -> m Unit
hangOutTillBlock provider logger bn = do
  bn' <- assertWeb3 provider Api.eth_blockNumber
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
  n <- assertWeb3 provider Api.eth_blockNumber
  let
    next = wrap $ one + unwrap n
  logger $ "Awaiting block number " <> show next
  hangOutTillBlock provider logger next

joinWeb3Fork
  :: forall a m
   . MonadAff m
  => Fiber (Either Web3Error a)
  -> m a
joinWeb3Fork fiber =
  liftAff do
    eRes <- joinFiber fiber
    case eRes of
      Left e -> unsafeCrashWith $ "Error in forked web3 process " <> show e
      Right a -> pure a

mkHexString'
  :: String
  -> HexString
mkHexString' hx = unsafePartial fromJust $ mkHexString hx

defaultTestTxOptions :: TransactionOptions NoPay
defaultTestTxOptions = defaultTransactionOptions # _gas ?~ bigGasLimit

nullAddress :: Address
nullAddress = unsafePartial $ fromJust $ mkAddress =<< mkHexString "0000000000000000000000000000000000000000"

bigGasLimit :: BigNumber
bigGasLimit = unsafePartial fromJust $ fromStringAs decimal "4712388"

deploy :: forall a. ContractConfig a -> DeployM { deployAddress :: Address, primaryAccount :: Address }
deploy contractConfig = do
  DeployConfig { primaryAccount } <- ask
  let txOpts = defaultTransactionOptions # _from ?~ primaryAccount
  { deployAddress } <- deployContract txOpts contractConfig
  pure { deployAddress, primaryAccount }

nodeUrl :: String
nodeUrl = "http://localhost:8545"
