module Web3Spec.Live.FilterSpec (spec) where

import Prelude

import Chanterelle.Utils (pollTransactionReceipt)
import Chanterelle.Test (assertWeb3, buildTestConfig)
import Contract.SimpleStorage as SimpleStorage
import Control.Monad.Reader (ask)
import Control.Monad.Trans.Class (lift)
import Data.Array ((..), snoc, length, head, sortWith)
import Data.Either (either)
import Data.Lens ((?~), (.~), (^.))
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (wrap, unwrap, un)
import Data.Ord.Down (Down(..))
import Data.Traversable (traverse_)
import Effect.AVar as EAVar
import Effect.Aff (Aff, Fiber, error, joinFiber)
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Network.Ethereum.Web3 (Address, BlockNumber(..), ChainCursor(..), Change(..), EventAction(..), Filter, Provider, UIntN, _from, _fromBlock, _to, _toBlock, event', eventFilter, forkWeb3, fromInt, throwWeb3, uIntNFromBigNumber)
import Network.Ethereum.Web3.Api as Api
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
import Test.Spec (SpecT, before, describe, it, parallel)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))
import Web3Spec.Live.ContractConfig as ContractConfig
import Web3Spec.Live.ContractUtils (Logger, defaultTestTxOptions, deployScript, go, hangOutTillBlock, nodeUrl)

spec :: SpecT Aff Unit Aff Unit
spec =
  let
    env =
      { logger: \s -> ask >>= \logger -> liftAff $ logger s
      }
  in
    go $ spec' env

type FilterEnv m =
  { logger :: String -> m Unit
  }

{-
NOTE: none of the Futures use Pending, the behavior is currently ill defined

Case [Past,Past] : The filter is starting and ending in the past.
Case [Past, ∞] : The filter starts in the past but continues indefinitely into the future.
Case [Future, ∞] : The fitler starts in the future and continues indefinitely into the future.
Case [Future, Future] : The fitler starts in the future and ends at a later time in the future.
-}
spec'
  :: forall m
   . MonadAff m
  => FilterEnv m
  -> SpecT m Unit Aff Unit
spec' { logger } = do
  uIntV <- liftEffect $ EAVar.new 1
  let gen = mkUIntsGen uIntV
  describe "Filters"
    $ parallel do
        before (deployUniqueSimpleStorage nodeUrl logger gen)
          $ it "Case [Past, Past]" \simpleStorageCfg -> do
              let
                { simpleStorageAddress, setter, uIntsGen, provider } = simpleStorageCfg

                filter = eventFilter (Proxy :: Proxy SimpleStorage.CountSet) simpleStorageAddress
              values <- uIntsGen 3
              logger $ "Searching for values " <> show values
              fiber <- monitorUntil provider logger filter (_ == aMax values) defaultFilterOpts
              start <- liftAff $ assertWeb3 provider Api.eth_blockNumber
              traverse_ setter values
              { endingBlockV } <- liftAff $ joinFiber fiber
              end <- liftAff $ AVar.take endingBlockV
              let
                pastFilter =
                  eventFilter (Proxy :: Proxy SimpleStorage.CountSet) simpleStorageAddress
                    # _fromBlock
                        .~ BN start
                    # _toBlock
                        .~ BN end
              fiber' <- monitorUntil provider logger pastFilter (const false) defaultFilterOpts
              { foundValuesV } <- liftAff $ joinFiber fiber'
              foundValues <- liftAff $ AVar.take foundValuesV
              liftAff $ foundValues `shouldEqual` values
        before (deployUniqueSimpleStorage nodeUrl logger gen)
          $ do
              it "Case [Past, ∞] No Trail" \simpleStorageCfg@{ provider } -> do
                fromPastToFutureTrailingBy simpleStorageCfg provider logger defaultFilterOpts
              it "Case [Past, ∞] With Trail" \simpleStorageCfg@{ provider } -> do
                fromPastToFutureTrailingBy simpleStorageCfg provider logger { trailBy: 3, windowSize: 2 }
        before (deployUniqueSimpleStorage nodeUrl logger gen)
          $ it "Case [Future, ∞]" \simpleStorageCfg -> do
              let
                { simpleStorageAddress, setter, uIntsGen, provider } = simpleStorageCfg
              values <- uIntsGen 3
              logger $ "Searching for values " <> show values
              now <- liftAff $ assertWeb3 provider Api.eth_blockNumber
              let
                later = wrap $ unwrap now + fromInt 3

                filter =
                  eventFilter (Proxy :: Proxy SimpleStorage.CountSet) simpleStorageAddress
                    # _fromBlock
                        .~ BN later
                    # _toBlock
                        .~ Latest
              fiber <- monitorUntil provider logger filter (_ == aMax values) defaultFilterOpts
              hangOutTillBlock provider logger later
              traverse_ setter values
              { foundValuesV } <- liftAff $ joinFiber fiber
              foundValues <- liftAff $ AVar.take foundValuesV
              liftAff $ foundValues `shouldEqual` values
        before (deployUniqueSimpleStorage nodeUrl logger gen)
          $ it "Case [Future, Future]" \simpleStorageCfg -> do
              let
                { simpleStorageAddress, setter, uIntsGen, provider } = simpleStorageCfg
              values <- uIntsGen 3
              logger $ "Searching for values " <> show values
              let
                nValues = length values
              now <- liftAff $ assertWeb3 provider Api.eth_blockNumber
              let
                later = wrap $ unwrap now + fromInt 3

                -- NOTE: This isn't that clean, but 2 blocks per set should be enough time
                latest = wrap $ unwrap later + fromInt (2 * nValues)

                filter =
                  eventFilter (Proxy :: Proxy SimpleStorage.CountSet) simpleStorageAddress
                    # _fromBlock
                        .~ BN later
                    # _toBlock
                        .~ BN latest
              fiber <- monitorUntil provider logger filter (_ == aMax values) defaultFilterOpts
              hangOutTillBlock provider logger later
              traverse_ setter values
              { foundValuesV } <- liftAff $ joinFiber fiber
              foundValues <- liftAff $ AVar.take foundValuesV
              liftAff $ foundValues `shouldEqual` values

--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------
type FilterOpts =
  { trailBy :: Int
  , windowSize :: Int
  }

defaultFilterOpts :: FilterOpts
defaultFilterOpts = { trailBy: 0, windowSize: 0 }

fromPastToFutureTrailingBy
  :: forall m
   . MonadAff m
  => SimpleStorageCfg m
  -> Provider
  -> Logger m
  -> FilterOpts
  -> m Unit
fromPastToFutureTrailingBy simpleStorageCfg provider logger opts = do
  let
    { simpleStorageAddress, setter, uIntsGen } = simpleStorageCfg

    filter1 = eventFilter (Proxy :: Proxy SimpleStorage.CountSet) simpleStorageAddress
  firstValues <- uIntsGen 7
  secondValues <- uIntsGen 7
  let
    allValues = firstValues <> secondValues
  logger $ "Searching for values " <> show allValues
  fiber1 <- monitorUntil provider logger filter1 (_ == aMax firstValues) defaultFilterOpts
  start <- liftAff $ assertWeb3 provider Api.eth_blockNumber
  traverse_ setter firstValues
  _ <- liftAff $ joinFiber fiber1
  let
    filter2 =
      eventFilter (Proxy :: Proxy SimpleStorage.CountSet) simpleStorageAddress
        # _fromBlock
            .~ BN start
        # _toBlock
            .~ Latest
  fiber2 <- monitorUntil provider logger filter2 (_ == aMax secondValues) opts
  traverse_ setter secondValues
  { foundValuesV, reachedTargetTrailByV } <- liftAff (joinFiber fiber2)
  foundValues <- liftAff $ AVar.take foundValuesV
  liftAff $ foundValues `shouldEqual` allValues
  when (opts.trailBy > 0) do
    reachedTargetTrailBy <- liftAff $ AVar.take reachedTargetTrailByV
    logger $ "Reached 'chainHead - trailBy' : " <> show reachedTargetTrailBy
    liftAff $ reachedTargetTrailBy `shouldEqual` true

monitorUntil
  :: forall m
   . MonadAff m
  => Provider
  -> Logger m
  -> Filter SimpleStorage.CountSet
  -> (UIntN 256 -> Boolean)
  -> FilterOpts
  -> m
       ( Fiber
           { endingBlockV :: AVar.AVar BlockNumber
           , foundValuesV :: AVar.AVar (Array (UIntN 256))
           , reachedTargetTrailByV :: AVar.AVar Boolean
           }
       )
monitorUntil provider logger filter p opts = do
  endingBlockV <- liftAff AVar.empty
  foundValuesV <- liftAff $ AVar.new []
  reachedTargetTrailByV <- liftAff $ AVar.new false
  logger $ "Creating filter with fromBlock="
    <> show (filter ^. _fromBlock)
    <> " toBlock="
    <> show (filter ^. _toBlock)
  let
    handler (SimpleStorage.CountSet { _count }) = do
      Change c <- ask
      chainHead <- lift Api.eth_blockNumber
      when (un BlockNumber chainHead - un BlockNumber c.blockNumber < fromInt opts.trailBy)
        $ lift
        $ throwWeb3
        $ error "Exceded max trailBy"
      when (un BlockNumber chainHead - un BlockNumber c.blockNumber == fromInt opts.trailBy) do
        _ <- liftAff $ AVar.take reachedTargetTrailByV
        liftAff $ AVar.put true reachedTargetTrailByV
      foundSoFar <- liftAff $ AVar.take foundValuesV
      liftAff $ AVar.put (foundSoFar `snoc` _count) foundValuesV
      let
        shouldTerminate = p _count
      if shouldTerminate then do
        liftAff $ AVar.put c.blockNumber endingBlockV
        pure TerminateEvent
      else
        pure ContinueEvent
  res <- liftAff $ forkWeb3 provider
    $ do
        _ <- event' { ev: filter } { ev: handler } opts
        pure { endingBlockV, foundValuesV, reachedTargetTrailByV }
  pure $ either (unsafeCrashWith <<< show) identity <$> res

type SimpleStorageCfg m =
  { simpleStorageAddress :: Address
  , setter :: UIntN 256 -> m Unit
  , uIntsGen :: Int -> m (Array (UIntN 256))
  , provider :: Provider
  }

deployUniqueSimpleStorage
  :: forall m
   . MonadAff m
  => String
  -> Logger m
  -> (Int -> m (Array (UIntN 256)))
  -> m (SimpleStorageCfg m)
deployUniqueSimpleStorage nodeUrl logger uIntsGen = liftAff $ do
  { deployAddress, provider, primaryAccount } <- buildTestConfig nodeUrl 60 $ deployScript ContractConfig.simpleStorageCfg

  let
    setter _count = do
      let
        txOptions =
          defaultTestTxOptions # _from ?~ primaryAccount
            # _to ?~ deployAddress
      logger $ "Setting count to " <> show _count
      txHash <- liftAff $ assertWeb3 provider $ SimpleStorage.setCount txOptions { _count }
      void $ pollTransactionReceipt txHash provider
  pure
    { simpleStorageAddress: deployAddress
    , setter
    , uIntsGen
    , provider
    }

mkUIntsGen
  :: forall m
   . MonadAff m
  => AVar.AVar Int
  -> Int
  -> m (Array (UIntN 256))
mkUIntsGen uintV n =
  liftAff do
    firstAvailable <- AVar.take uintV
    let
      nextVal = firstAvailable + n

      res = firstAvailable .. (nextVal - 1)
    AVar.put nextVal uintV
    pure $ map (\x -> unsafePartial $ fromJust $ uIntNFromBigNumber (Proxy @256) $ fromInt x) res

aMax :: forall a. Ord a => Array a -> a
aMax as = case head $ sortWith Down as of
  Nothing -> unsafeCrashWith "Can't take the max of an empty array"
  Just a -> a
