module PayableSpec (payableTestSpec) where

import Prelude

import Chanterelle.Test (TestConfig)
import Contracts.PayableTest as PayableTest
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Data.Array ((!!))
import Data.Either (fromRight)
import Data.Lens.Setter ((.~))
import Data.Maybe (Maybe(..), fromJust)
import Network.Ethereum.Web3 (Ether, EventAction(..), Shannon, Value, _from, _to, _value, convert, defaultTransactionOptions, event, eventFilter, mkValue, runWeb3, unUIntN, Address)
import Partial.Unsafe (unsafePartial)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))


payableTestSpec :: forall r. TestConfig (payableTest :: Address | r) -> Spec Unit
payableTestSpec {provider, accounts, payableTest} =
  describe "interacting with a PayableTest contract" do
    it "can send the right amount of wei" $ do
      let primaryAccount = unsafePartial $ fromJust $ accounts !! 0
      var <- AVar.empty
      let txOptions = defaultTransactionOptions # _to .~ Just payableTest
                                                # _value .~ Just (convert (mkValue one :: Value Ether))
                                                # _from .~ Just primaryAccount
      _ <- map (unsafePartial fromRight) <<< runWeb3 provider $ PayableTest.seeContent txOptions
      let filterContent = eventFilter (Proxy :: Proxy PayableTest.Content) payableTest
      _ <- liftAff $ runWeb3 provider $
        event filterContent $ \e@(PayableTest.Content c) -> do
          liftEffect $ log $ "Received Event: " <> show e
          _ <- liftAff $ AVar.put c._paidContent var
          pure TerminateEvent
      val <- unUIntN <$> AVar.take var
      Just val `shouldEqual` Just one
      let txOptions' = defaultTransactionOptions # _to .~ Just payableTest
                                                 # _value .~ Just (convert (mkValue one :: Value Shannon))
                                                 # _from .~ Just primaryAccount
      _ <- map (unsafePartial fromRight) <<< runWeb3 provider $ PayableTest.seeContent txOptions'
      let filterContent' = eventFilter (Proxy :: Proxy PayableTest.Content) payableTest
      _ <- liftAff $ runWeb3 provider $
        event filterContent' $ \e@(PayableTest.Content c) -> do
          liftEffect $ log $ "Received Event: " <> show e
          _ <- liftAff $ AVar.put c._paidContent var
          pure TerminateEvent
      val' <- unUIntN <$> AVar.take var
      Just val' `shouldEqual` Just zero
