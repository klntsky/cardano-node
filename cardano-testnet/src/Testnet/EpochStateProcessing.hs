{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Testnet.EpochStateProcessing
  ( maybeExtractGovernanceActionIndex
  , findCondition
  ) where

import           Cardano.Api
import           Cardano.Api.Ledger (GovActionId (..))
import qualified Cardano.Api.Ledger as L

import qualified Cardano.Ledger.Conway.Governance as L
import qualified Cardano.Ledger.Shelley.API as L
import qualified Cardano.Ledger.Shelley.LedgerState as L

import           Prelude

import           Control.Monad.State.Strict (MonadState (put), StateT)
import           Data.Data ((:~:) (..))
import qualified Data.Map as Map
import           Data.Word (Word32)
import           GHC.Stack
import           Lens.Micro ((^.))

import           Testnet.Property.Assert (assertErasEqual)

import           Hedgehog

findCondition
  :: HasCallStack
  => MonadTest m
  => MonadIO m
  => (AnyNewEpochState -> Maybe a)
  -> FilePath
  -> FilePath
  -> EpochNo -- ^ The termination epoch: the condition must be found *before* this epoch
  -> m (Either FoldBlocksError (Maybe a))
findCondition epochStateFoldFunc configurationFile socketPath maxEpochNo = withFrozenCallStack $ evalIO . runExceptT $ do
  result <-
    foldEpochState
      (File configurationFile)
      (File socketPath)
      FullValidation
      maxEpochNo
      Nothing
      (\epochState _ _ -> go epochStateFoldFunc epochState)
  pure $ case result of
    (ConditionMet, Just x) -> Just x
    _                      -> Nothing

  where
    go :: (AnyNewEpochState -> Maybe a) -> AnyNewEpochState -> StateT (Maybe a) IO LedgerStateCondition
    go f epochState = do
      case f epochState of
        Just x -> put (Just x) >> pure ConditionMet
        Nothing -> pure ConditionNotMet

maybeExtractGovernanceActionIndex
  :: forall era. HasCallStack
  => ConwayEraOnwards era -- ^ The era in which the test runs
  -> TxId -- ^ transaction id searched for
  -> AnyNewEpochState
  -> Maybe Word32
maybeExtractGovernanceActionIndex ceo txid (AnyNewEpochState actualEra newEpochState) = conwayEraOnwardsConstraints ceo $ do
  let sbe = conwayEraOnwardsToShelleyBasedEra ceo
  Refl <- either error pure $ assertErasEqual sbe actualEra
  let proposals = newEpochState ^. L.newEpochStateGovStateL . L.proposalsGovStateL
  Map.foldlWithKey' (compareWithTxId txid) Nothing (L.proposalsActionsMap proposals)
  where
    compareWithTxId (TxId ti1) Nothing (GovActionId (L.TxId ti2) (L.GovActionIx gai)) _
      | ti1 == L.extractHash ti2 = Just gai
    compareWithTxId _ x _ _ = x

