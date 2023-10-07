module Test.Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Milliseconds(..), launchAff_)
import Test.Spec (parallel)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (defaultConfig, runSpecT)
import Web3Spec.Live.ComplexStorageSpec as ComplexStorageSpec
import Web3Spec.Live.FilterSpec as FilterSpec
import Web3Spec.Live.MockERC20Spec as MockERC20Spec
import Web3Spec.Live.MultifilterSpec as MultifilterSpec
import Web3Spec.Live.NestedTuplesSpec as NestedTuplesSpec
import Web3Spec.Live.PayableTestSpec as PayableTestSpec
import Web3Spec.Live.SimpleErrorTestSpec as SimpleErrorTestSpec
import Web3Spec.Live.SimpleStorageSpec as SimpleStorageSpec

main :: Effect Unit
main =
  launchAff_ do
    let
      cfg = defaultConfig { timeout = Just (Milliseconds $ 120.0 * 1000.0) }
    void $ join
      $ runSpecT cfg [ consoleReporter ] do
          parallel do
            -- all of these tests only have one `it` statement and
            -- are dealing with separate contracts so they can be run
            -- in parallel
            SimpleStorageSpec.spec
            NestedTuplesSpec.spec
            ComplexStorageSpec.spec
            MockERC20Spec.spec
            SimpleErrorTestSpec.spec
          --MultifilterSpec.spec
          --FilterSpec.spec
          -- payable spec can't be run in parallel :/
          PayableTestSpec.spec
