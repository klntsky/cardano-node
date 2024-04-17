{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Testnet.Test.LedgerEvents.Gov.PredefinedAbstainDRep
  ( hprop_check_predefined_abstain_drep
  ) where

import           Cardano.Api as Api

import           Cardano.Testnet

import           Prelude

import           System.FilePath ((</>))

import           Testnet.Components.Query (getEpochStateView)
import qualified Testnet.Process.Run as H
import qualified Testnet.Property.Utils as H
import           Testnet.Runtime

import           Hedgehog
import qualified Hedgehog.Extras as H
import qualified Hedgehog.Extras.Stock.IO.Network.Sprocket as IO

-- | Execute me with:
-- @DISABLE_RETRIES=1 cabal test cardano-testnet-test --test-options '-p "/Predefined Abstain DRep/"'@
hprop_check_predefined_abstain_drep :: Property
hprop_check_predefined_abstain_drep = H.integrationWorkspace "test-activity" $ \tempAbsBasePath' -> do
    -- Start a local test net
  conf@Conf { tempAbsPath } <- mkConf tempAbsBasePath'
  let tempAbsPath' = unTmpAbsPath tempAbsPath
      tempBaseAbsPath = makeTmpBaseAbsPath tempAbsPath

  work <- H.createDirectoryIfMissing $ tempAbsPath' </> "work"

  -- Create default testnet with 3 DReps and 3 stake holders delegated, one to each DRep.
  let sbe = ShelleyBasedEraConway
      era = toCardanoEra sbe
      cEra = AnyCardanoEra era
      fastTestnetOptions = cardanoDefaultTestnetOptions
        { cardanoEpochLength = 100
        , cardanoNodeEra = cEra
        , cardanoNumDReps = 3
        }

  testnetRuntime@TestnetRuntime
    { testnetMagic
    , poolNodes
    , wallets=_wallet0:_wallet1:_wallet2:_
    , configurationFile
    }
    <- cardanoTestnetDefault fastTestnetOptions conf

  poolNode1 <- H.headM poolNodes
  poolSprocket1 <- H.noteShow $ nodeSprocket $ poolRuntime poolNode1
  _execConfig <- H.mkExecConfig tempBaseAbsPath poolSprocket1 testnetMagic

  let socketName' = IO.sprocketName poolSprocket1
      socketBase = IO.sprocketBase poolSprocket1 -- /tmp
      socketPath = socketBase </> socketName'

  _epochStateView <- getEpochStateView (File configurationFile) (File socketPath)

  startLedgerNewEpochStateLogging testnetRuntime tempAbsPath'

  H.note_ $ "Sprocket: " <> show poolSprocket1
  H.note_ $ "Abs path: " <> tempAbsBasePath'
  H.note_ $ "Socketpath: " <> socketPath
  H.note_ $ "Foldblocks config file: " <> configurationFile

  _gov <- H.createDirectoryIfMissing $ work </> "governance"

  -- ToDo: Do some proposal and vote yes with the first DRep only.
  -- ToDo: ASSERT: Check that proposal does NOT pass.
  -- ToDo: Take the last two stake delegators and delegate them to "Abstain".
  -- ToDo: This can be done using cardano-cli conway stake-address vote-delegation-certificate --always-abstain
  -- ToDo: Do some other proposal and vote yes with first DRep only.
  -- ToDo: ASSERT: Check the new proposal passes now.

  success
